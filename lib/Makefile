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

BOOST_LIBS := -lboost_python3 -lpython3.5m

all:
	clang++ -shared -o ../cppparser.so -fpic CPPParser.cpp -std=c++14 \
	$(CLANG_LIBS) $(BOOST_LIBS) \
	$(LLVM_LDFLAGS) $(PYTHON_CXXFLAGS)

debug:
	clang++ -shared -o ../cppparser.so -g -fpic CPPParser.cpp -std=c++14 -DDEBUG \
	$(CLANG_LIBS) $(BOOST_LIBS) \
	$(LLVM_LDFLAGS) $(PYTHON_CXXFLAGS)
