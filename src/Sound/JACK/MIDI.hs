module Sound.JACK.MIDI
  ( RawEvent,
    rawEvent,
    rawEventTime,
    rawEventBuffer,
    toRawEventFunction,
    Port,
    withPort,
    withProcess,
    Buffer,
    getBuffer,
    clearBuffer,
    readRawEvents,
    writeRawEvent,
    readRawEventsFromPort,
    writeRawEventsToPort,
    writeEvent,
    readEventsFromPort,
    writeEventsToPort,
    main,
    mainRaw,
  )
where

import Control.Monad (forM)
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.EventList.Absolute.TimeBody as EventList
import Data.Maybe (mapMaybe)
import Foreign.C.Error (Errno)
import Foreign.Storable (peek)
import Sound.JACK (Direction, Input, Output)
import qualified Sound.JACK as Jack
import qualified Sound.JACK.Exception as JackExc
import Sound.JACK.FFI (NFrames, nframesIndices)
import qualified Sound.JACK.FFI as JackFFI
import Sound.JACK.FFI.MIDI
  ( Buffer (Buffer),
    EventBuffer,
    RawEvent,
    toRawEventFunction,
  )
import qualified Sound.JACK.FFI.MIDI as JackMIDI
import Sound.JACK.Private (Client, alloca, liftErrno)
import qualified Sound.JACK.Private as Priv
import qualified Sound.MIDI.Message as Msg
import qualified Sound.MIDI.Parser.Report as Report
import System.Environment (getProgName)

type Port = Priv.Port EventBuffer

withPort ::
  ( Direction dir,
    JackExc.ThrowsPortRegister e,
    JackExc.ThrowsErrno e
  ) =>
  -- | Jack client
  Client ->
  -- | name of the input port
  String ->
  (Port dir -> Sync.ExceptionalT e IO a) ->
  Sync.ExceptionalT e IO a
withPort = Jack.withPort

-- |
-- Smart constructor for a raw MIDI event.
rawEvent ::
  -- | Sample index at which event is valid (relative to cycle start)
  NFrames ->
  -- | Raw MIDI data
  B.ByteString ->
  RawEvent
rawEvent = JackMIDI.RawEvent

rawEventTime :: RawEvent -> NFrames
rawEventTime = JackMIDI.time

rawEventBuffer :: RawEvent -> B.ByteString
rawEventBuffer = JackMIDI.buffer

-- | Creates an input and an output, and transforms all raw input events into raw output
--   events using the given function
mainRaw ::
  -- | transforms raw input to output events
  (NFrames -> RawEvent -> IO RawEvent) ->
  IO ()
mainRaw fun = do
  name <- getProgName
  Jack.handleExceptions $
    Jack.withClientDefault name $ \client ->
      Jack.withPort client "input" $ \input ->
        Jack.withPort client "output" $ \output ->
          withProcess client input fun output $
            Jack.withActivation client $
              Trans.lift $ do
                putStrLn $ "started " ++ name ++ "..."
                Jack.waitForBreak

-- | Creates an input and an output, and transforms all input events into output
--   events using the given function
main ::
  -- | transforms input to output events
  (NFrames -> (NFrames, Msg.T) -> IO (NFrames, Msg.T)) ->
  IO ()
main fun = mainRaw (toRawEventFunction fun)

-- | sets the process loop of the JACK Client
withProcess ::
  (JackExc.ThrowsErrno e) =>
  -- | the JACK Client, whose process loop will be set
  Client ->
  -- | where to get events from
  Port Input ->
  -- | transforms input to output events
  (NFrames -> RawEvent -> IO RawEvent) ->
  -- | where to put events
  Port Output ->
  Sync.ExceptionalT e IO a ->
  -- | exception causing JACK to remove that client from the process() graph.
  Sync.ExceptionalT e IO a
withProcess client input fun output =
  Jack.withProcess client $ wrapFun client input fun output

-- | reads all available MIDI Events on the given PortBuffer
readRawEvents ::
  (JackExc.ThrowsErrno e) =>
  -- | the PortBuffer to read from
  Buffer Input ->
  Sync.ExceptionalT e IO [RawEvent]
readRawEvents buffer_ = do
  nEvents <- Trans.lift $ JackMIDI.get_event_count buffer_
  forM (nframesIndices nEvents) $ \n ->
    alloca $ \eventPtr -> do
      liftErrno $ JackMIDI.event_get eventPtr buffer_ n
      Trans.lift $ peek eventPtr

-- putStrLn $ "Got MIDI Event number " ++ (show n) ++ " with result " ++ (show result) ++ " at JackMIDI.time " ++ (show $ JackMIDI.time event) ++ " size: " ++ (show $ size event) ++ " JackMIDI.buffer: " ++ (show $ JackMIDI.buffer event)

getBuffer ::
  (Direction dir) =>
  Port dir ->
  NFrames ->
  IO (Buffer dir)
getBuffer (Priv.Port port) n =
  Buffer <$> JackFFI.port_get_buffer port n

clearBuffer :: Buffer Output -> IO ()
clearBuffer portBuffer =
  JackMIDI.clear_buffer portBuffer

-- | writes a MIDI event to the PortBuffer of a MIDI output or throws eNOBUFS if JackMIDI.buffer is full
writeRawEvent ::
  (JackExc.ThrowsErrno e) =>
  -- | the PortBuffer of the MIDI output to write to
  Buffer Output ->
  -- | the RawEvent to write
  RawEvent ->
  Sync.ExceptionalT e IO ()
writeRawEvent portBuffer event =
  liftErrno $
    JackMIDI.withByteStringPtr (JackMIDI.buffer event) $ \ptr len ->
      JackMIDI.event_write
        portBuffer
        (JackMIDI.time event)
        ptr
        (fromIntegral len)

-- putStrLn $ "Writing MIDI Event: JackMIDI.buffer: " ++ (show $ JackMIDI.buffer event)

readRawEventsFromPort ::
  (JackExc.ThrowsErrno e) =>
  Port Input ->
  NFrames ->
  Sync.ExceptionalT e IO [RawEvent]
readRawEventsFromPort port nframes =
  readRawEvents =<< Trans.lift (getBuffer port nframes)

-- |
-- Clears an output buffer and writes a sequence of events to it.
-- That is, you must only call this function once per callback.
writeRawEventsToPort ::
  (JackExc.ThrowsErrno e) =>
  Port Output ->
  NFrames ->
  [RawEvent] ->
  Sync.ExceptionalT e IO ()
writeRawEventsToPort port nframes es = do
  buffer <- Trans.lift $ getBuffer port nframes
  Trans.lift $ clearBuffer buffer
  mapM_ (writeRawEvent buffer) es

parseEvent :: JackMIDI.RawEvent -> Maybe (NFrames, Msg.T)
parseEvent ev =
  case Msg.maybeFromByteString $ BL.fromChunks [rawEventBuffer ev] of
    Report.Cons _warnings result ->
      case result of
        Right dat -> Just (rawEventTime ev, dat)
        _ -> Nothing

-- |
-- Reads midi events from an input buffer
-- and converts them to a high-level representation.
-- Messages are simply ignored if they cannot be parsed.
readEventsFromPort ::
  (JackExc.ThrowsErrno e) =>
  Port Input ->
  NFrames ->
  Sync.ExceptionalT e IO (EventList.T NFrames Msg.T)
readEventsFromPort port nframes =
  fmap (EventList.fromPairList . mapMaybe parseEvent) $
    readRawEventsFromPort port nframes

writeEvent ::
  (JackExc.ThrowsErrno e) =>
  Buffer Output ->
  NFrames ->
  Msg.T ->
  Sync.ExceptionalT e IO ()
writeEvent buffer pos =
  writeRawEvent buffer
    . JackMIDI.RawEvent pos
    . B.concat
    . BL.toChunks
    . Msg.toByteString

-- |
-- Clears an output buffer and writes a sequence of events to it.
-- That is, you must only call this function once per callback.
writeEventsToPort ::
  (JackExc.ThrowsErrno e) =>
  Port Output ->
  NFrames ->
  EventList.T NFrames Msg.T ->
  Sync.ExceptionalT e IO ()
writeEventsToPort port nframes =
  writeRawEventsToPort port nframes
    . map (uncurry JackMIDI.RawEvent)
    . EventList.toPairList
    . fmap (B.concat . BL.toChunks . Msg.toByteString)

wrapFun ::
  Client ->
  Port Input ->
  (NFrames -> RawEvent -> IO RawEvent) ->
  Port Output ->
  NFrames ->
  Sync.ExceptionalT Errno IO ()
wrapFun client input fun output nframes = do
  inEvents <- readRawEventsFromPort input nframes
  -- when (not (null inEvents)) $ putStrLn $ "wrapFun: Got " ++ (show $ length inEvents) ++ " input events"
  lastCycle <- Trans.lift $ Jack.lastFrameTime client
  outEvents <- mapM (Trans.lift . fun lastCycle) inEvents
  -- when (not (null outEvents)) $ putStrLn $ "wrapFun: Got " ++ (show $ length outEvents) ++ " output events"
  writeRawEventsToPort output nframes outEvents
