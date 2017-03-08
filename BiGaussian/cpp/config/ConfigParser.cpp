#pragma once
#include "ConfigParser.h"
#include <fstream>
#include <vector>
using namespace std;
string trim(string const& source, char const* delims = " \t\r\n ") {
	string result(source);
	string::size_type index = result.find_last_not_of(delims);
	if(index != string::npos)
		result.erase(++index);

	index = result.find_first_not_of(delims);
	if(index != string::npos)
		result.erase(0, index);
	else
		result.erase();
	return result;
}

ConfigParser::ConfigParser(string const& ConfigParser) {
	ifstream file(ConfigParser.c_str());
	string line;
	string name;
	string value;
	int posEqual;
	while (std::getline(file,line)) {
		if (! line.length()) continue;
		if (line[0] == '#') continue;
		if (line[0] == ';') continue;

		posEqual=line.find('=');
		name  = trim(line.substr(0,posEqual));
		value = trim(line.substr(posEqual+1));
		
		content_[name] = SwapStringDouble(value);
	}
}

SwapStringDouble const& ConfigParser::Value(string const& fieldName) const {
	map<string, SwapStringDouble>::const_iterator ci = content_.find(fieldName);
	if (ci == content_.end()) throw fieldName + " does not exist!";
	return ci->second;	
}

SwapStringDouble const& ConfigParser::Value(string const& fieldName, double value) {
	try {
		return Value(fieldName);
	} catch(const char *) {
		return content_.insert(make_pair(fieldName, SwapStringDouble(value))).first->second;
	}
}

SwapStringDouble const& ConfigParser::Value(string const& fieldName, string const& value) {
	try {
		return Value(fieldName);
	} catch(const char *) {
		return content_.insert(make_pair(fieldName, SwapStringDouble(value))).first->second;
	}
}

void splitNumeric(string str_src, const char *separator, vector<float> &destFloat)
{
	if (str_src[0] == '[' && str_src[str_src.length()-1] == ']')
	{
		str_src = trim(str_src.substr(1,str_src.find(']')-1));
	}
	char* src;
	src = new char[str_src.length()+1];
	strcpy(src, str_src.c_str());
	char *pNext;
	int count = 0;
	if(src == NULL || strlen(src) == 0) return;
	if(separator == NULL || strlen(separator) == 0) return;
	pNext = strtok(src,separator);
	while (pNext != NULL)
	{
		destFloat.push_back(atof(trim(pNext).c_str()));
		++count;
		pNext = strtok(NULL,separator);
	}
	vector<float> (destFloat).swap(destFloat);
}