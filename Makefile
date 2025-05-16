# Makefile for Rational Enumeration Project

GHC=ghc
SRC=Main.hs
TARGET=run_enum

all: $(TARGET)

$(TARGET): $(SRC)
	$(GHC) -o $(TARGET) $(SRC)

run: $(TARGET)
	./$(TARGET)

clean:
	rm -f $(TARGET) *.hi *.o
