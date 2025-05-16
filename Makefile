# Makefile for Rational Enumeration Project

GHC=ghc
SRC=Main.hs rats6.hs
TARGET=run_enum
RATS6=rats6
RATS4=rats4

all: $(TARGET) $(RATS6) $(RATS4)

$(TARGET): Main.hs
	$(GHC) -O2 -rtsopts -with-rtsopts=-T -o $(TARGET) Main.hs

$(RATS6): rats6.hs
	$(GHC) -O2 -o $(RATS6) rats6.hs

$(RATS4): rats4.hs
	$(GHC) -O2 -o $(RATS4) rats4.hs

clean:
	rm -f $(TARGET) $(RATS6) $(RATS4) *.hi *.o