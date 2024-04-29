ghci:
	ghci -Wall -ljack -i:src:free:dist/build src/Sound/JACK/Exception.hs

test:
	runhaskell Setup configure --user -fbuildExamples -f-pkgConfig -f-jackFree
	runhaskell Setup build
	runhaskell Setup haddock

jack:	jack2

jack1:
# -n 2 is default, larger values are not supported
# --period 1024  is default, option is mentioned in manpage but jackd does not accept it
# -r 44100
# --midi-bufsize 200
	jackd --nozombies --timeout 2000 -d alsa -Xseq -n 3 -d hw:0 &

jack2:
	jackd --timeout 2000 -d alsa -Xseq -n 3 -d hw:0 &
