#include "modular_arithmetic.h"
using namespace std;

num_type mod (num_type a, num_type b)
{
    return a >= 0 ? a % b 
                  : b + (a % b);
}


bool is_prime (num_type n)
{
	if (n == 2)
		return true;

	if (n < 2 or mod(n, 2) == 0)
		return false;

	for (num_type i = 3; i <= sqrt(n); ++i)
	{
		if (mod(n, i) == 0)
			return false;
	}

	return true;
}


num_type pow_mod_Fermats (num_type a, num_type x, num_type p)
{
	if (mod(a, p) == 0)
		throw invalid_argument("ошибка: a делиться на p");

	if (not is_prime(p))
		throw invalid_argument("ошибка: p - составное число");

	num_type result = 1;
	x = mod(x, p - 1);

	for (num_type i = 0; i < x; ++i)
	{
		result = mod(result * a, p);
	}

	return result;
}


num_type pow_mod(num_type a, num_type x, num_type p)
{
    num_type result = 1;

    while (x > 0)
    {
        if (mod(x, 2) == 1)
            result = mod(result * a, p);

        a = mod(a * a, p);
		x /= 2;
    }

    return result;
}


tuple<num_type, num_type, num_type>
nod_extended (num_type a, num_type b)
{
	num_type vPrev = 1, uPrev = 0;
	num_type v = 0, u = 1;
	num_type r;

	while ((r = mod(a, b)) != 0)
	{
		num_type tV = vPrev;
		num_type tU = uPrev;

		vPrev = v;
		uPrev = u;

		v = tV - (a / b) * vPrev;
		u = tU - (a / b) * uPrev;

		a = b, b = r;
	}

	return make_tuple(v, u, b);
}