CXX = clang++
LLVM = -I$(shell llvm-config-3.2 --includedir) -lLLVM-3.2
CALCULON = -I/home/dg/nonshared/calculon/include

all: terrainmaker

terrainmaker: terrainmaker.cc Makefile $(wildcard *.h)
	$(CXX) -std=c++11 -O3 -o $@ $< $(CALCULON) \
		$(LLVM) \
		-lpng12 \
		-lnoise \
		-lboost_program_options \
		-lboost_iostreams
	
	
