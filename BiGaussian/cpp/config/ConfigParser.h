#pragma once
#ifndef CONFIG_PARSER
#define CONFIG_PARSER
#include <string>
#include <map>
#include <vector>
#include "SwapStringDouble.h"
#define MAXLN 255
using namespace std;
class ConfigParser{
	map<string, SwapStringDouble> content_;
public:
	ConfigParser(string const& configFile);
	SwapStringDouble const& Value(string const& fieldName) const;
	SwapStringDouble const& Value(string const& fieldName, double value);
	SwapStringDouble const& Value(string const& fieldName, string const& value);
};

void splitNumeric(string str_src, const char *separator, vector<float> &destFloat);
#endif 
