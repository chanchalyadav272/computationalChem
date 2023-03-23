#include <bits/stdc++.h>
using namespace std;

double m = 1, hbar = 1, delx = 0.01, dele = 0.1, xmin = 0, xmax = 10, emin = 1, emax = 26, k, pavg;
complex<double> z(0, -1), psi[1001];
double x[1001], V[1001], E[251], pd[1001], t[251];

void wave(double a)
{

    for (int j = 0; j <= 1000; j++)
    {
        x[j] = j * delx;

        // if (x[j] >= 4 && x[j] <= 5)
        // {
        //     V[j] = 9;
        // }
        // else
        // {
        //     V[j] = 0;
        // }
        if(x[j]<=4.5) V[j] = 9*exp(-1*pow((x[j]-4.5)/0.6,2));
        else V[j] = 9*exp(-1*pow((x[j]-4.5)/0.6,2))+4;
    }
    psi[0] = 1;

    k = sqrt(2 * m * (a - V[1] / pow(hbar, 2)));
    psi[1] = exp(z * k * delx);

    for (int j = 1; j <= 999; j++)
    {
        psi[j + 1] = ((2 - (2 * m * (a - V[j]) * pow(0.01, 2) / pow(hbar, 2))) * psi[j]) - psi[j - 1];
    }

    for (int j = 0; j <= 1000; j++)
    {
        pd[j] = pow(abs(psi[j]), 2);
    }
}

int main()
{

    fstream file1, file2;
    file1.open("out1.txt", ios::out);
    file2.open("out2.txt", ios::out);

    wave(9.0);
    for (int i = 0; i <= 1000; i++)
    {
        file1 << x[i] << " " << pd[i] << endl;
        
    }

    for (int i = 0; i <= 140; i++)
    {
        E[i] = 1 + i * dele;

        wave(E[i]);

        pavg = (*min_element(pd + 600, pd + 1001) + *max_element(pd + 600, pd + 1001)) / 2;

        t[i] = 2 / (1 + pavg);

        file2 << E[i] << " " << t[i] << endl;
    }

    return 0;
}