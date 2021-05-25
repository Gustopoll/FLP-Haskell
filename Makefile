NAME=dka-2-mka
CC=ghc

all: clean dka-2-mka

dka-2-mka:
	$(CC) -o $(NAME) Main.hs

clean:
	rm -f dka-2-mka *.hi *.o