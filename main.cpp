#include <iostream>
#include <fstream>
#include <string>
#include "shamirs_scheme.h"
using namespace std;

string file_to_string (const string& name)
{
	string res, s;
	ifstream in (name);

	while (getline(in, s))
	{
		res += (s + "\n");
	}

	in.close();

	return res;
}


int main()
{
	num_type prime = 3701;
	num_type p = 6025631;
	num_type q = 8478859;
	num_type secret = rand() % prime;

	int n, k;
	cout << "Введите n и k: ";
	cin >> n >> k;

	string fname;
	cout << "Введите имя файла: ";
	cin >> fname;
	
	vector<key_type> gen_keys = keys_generation(secret, n, k, prime);

	string text = file_to_string(fname);
	encode_and_decode(text, secret, p * q);

	ofstream encode ("encode.txt");
	encode << text;
	encode.close();

	cout << "\nКлючи:" << endl;
	for (auto [x, y] : gen_keys)
	{
		cout << x << " " << y << endl;
	}
	
	cout << "\nВведите " << k << " ключа" << endl;
	vector<key_type> keys(k);

	for (auto& [x, y] : keys)
	{
		cin >> x >> y;
	}

	secret = secret_recovery (keys, prime);
	encode_and_decode(text, secret, p * q);

	ofstream decode ("decode.txt");
	decode << text;
	decode.close();
}