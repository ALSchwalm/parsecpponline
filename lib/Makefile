LLVM_LDFLAGS := `llvm-config --ldflags --libs --system-libs`

PYTHON_CXXFLAGS := `python-config --includes`

CLANG_LIBS := \
	-Wl,--start-group \
	-lclangAST \
	-lclangAnalysis \
	-lclangBasic \
	-lclangDriver \
	-lclangEdit \
	-lclangFrontend \
	-lclangFrontendTool \
	-lclangLex \
	-lclangParse \
	-lclangSema \
	-lclangEdit \
	-lclangASTMatchers \
	-lclangRewrite \
	-lclangRewriteFrontend \
	-lclangStaticAnalyzerFrontend \
	-lclangStaticAnalyzerCheckers \
	-lclangStaticAnalyzerCore \
	-lclangSerialization \
	-lclangToolingCore \
	-lclangTooling \
	-Wl,--end-group

BOOST_LIBS :=  -lpython3 -lboost_python3

all:
	clang++ -shared -o ../cppparser.so -g -fpic CPPParser.cpp -std=c++11 \
	$(CLANG_LIBS) $(BOOST_LIBS) \
	$(LLVM_LDFLAGS) $(PYTHON_CXXFLAGS)
