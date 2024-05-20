#include "shamirs_scheme.h"
using namespace std;

vector<num_type> rand_unique_natural_nums (int k)
{
    set<num_type> nums;

    while (nums.size() < k)
    {
        nums.insert(rand() + 1);
    }

    return vector<num_type> (nums.begin(), nums.end());
}


vector<key_type>
keys_generation (num_type secret, int n, int k, num_type m)
{
	vector<num_type> xs = rand_unique_natural_nums (n);
	vector<num_type> coeffs = rand_unique_natural_nums (k - 1);
	coeffs.insert(coeffs.begin(), secret);
	
	vector<key_type> keys;

	for (auto x : xs)
	{
		num_type y = 0;

		for (size_t i = 0; i < coeffs.size(); ++i)
		{
			num_type mono = coeffs[i] * pow_mod(x, i, m);
			y = (y + mono) % m;
		}

		keys.push_back( {x, y} );
	}

	return keys;
}


num_type secret_recovery (const vector<key_type>& keys, num_type p)
{
	num_type secret = 0;

	for (auto [xJ, yJ] : keys)
	{
        num_type num = yJ;
		num_type denom = 1;

		for (auto [x, ignore_y] : keys)
		{
			if (x == xJ)
				continue;

			num = mod(num * -x, p);
            denom = denom * pow_mod(xJ - x, p - 2, p);
		}
		
		secret = mod(secret + num * denom, p);
	}

	return secret;
}


pair <char, num_type>
pseudo_rand_symbol (num_type x_n, num_type M)
{
	char rndS = 0;
	
	for (int i = 0; i < 8; ++i)
	{
		x_n = pow_mod (x_n, 2, M);
		rndS = rndS * 2 + (x_n % 2);
	}

	return {rndS, x_n};
}


void encode_and_decode (string& s, num_type secret_key, num_type M)
{
	num_type x_n = secret_key;

	for (auto& ch : s)
	{
		char rndS;
		tie(rndS, x_n) = pseudo_rand_symbol(x_n, M);
		ch = ch ^ rndS;
	}
}