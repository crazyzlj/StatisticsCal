/* BiGaussian fitting, rewrite from R package named apLCMS, which is develped by Dr. Tianwei YU (tianwei.yu@emory.edu).

  Liangjun, Zhu
  zlj@lreis.ac.cn
  Lreis, CAS  
  Feb 24, 2016 
  
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <fstream>
#include "stats.h"
#include "Config/ConfigParser.h"
using namespace std;

int main(int argc, char **argv)
{
	char inconfigfile[MAXLN],outconfigfile[MAXLN];
	if(argc != 3)
	{  
		printf("Error: Please set the right input configuration and output file!\n");
		exit(0);
	}
	else{
		strcpy(inconfigfile,argv[1]);
		strcpy(outconfigfile,argv[2]);
	}
	ConfigParser cf(inconfigfile);
	float frequency = cf.Value("frequency");
	float bandwidth = cf.Value("bandwidth");
	float max_iter = cf.Value("max_iter");
	float eliminate = cf.Value("eliminate");
	float epsilon = cf.Value("epsilon");
	string estim_method = cf.Value("estim_method");
	int estim_method_int;
	if (estim_method == "moment")
		estim_method_int = 0;
	else
		estim_method_int = 1;
	double power = cf.Value("power");
	string sigma_ratio_limit_str = cf.Value("sigma_ratio_limit");
	string x_str = cf.Value("x");
	string y_str = cf.Value("y");

	vector<float> x,y,sigma_ratio_limit;
	splitNumeric(x_str,",\t",x);
	splitNumeric(y_str,",\t",y);
	splitNumeric(sigma_ratio_limit_str,",",sigma_ratio_limit);
	pair<vector<float>,vector<int>> newy = which(y,1,frequency);
	vector<float> newx(newy.second.size());
	for (int i = 0; i < newy.second.size(); i++)
	{
		newx[i] = x[newy.second[i]];
	}

	vector<vector<float> > bigauss_results; 
	/// Be sure that x are ascend
	int bigauss = BiGaussianMix(newx,newy.first,sigma_ratio_limit,bandwidth,power,estim_method_int,eliminate,epsilon,max_iter, bigauss_results);
	/// End BiGaussian Fitting

	return 0;
}