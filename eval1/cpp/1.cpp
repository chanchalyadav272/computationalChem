#include <iostream>
#include <fstream>
#include <math.h>

using namespace std;

double Factorial(int n)
{
    int f = 1;
    if (n < 0)
    {
        cout << "INPUT FOR FACTORIAL MUST BE A NON NEGATIVE INTEGER" << endl;
    }
    else if (n == 0)
        f = 1;
    else
    {
        for (int i = 1; i <= n; i++)
        {
            f = f * i;
        }
    }
    return f;
}

void bessel(int a, double b, double j[], double n[])
{

    if (b <= 0.2)
    {
        j[0] = pow(b, 0) / Factorial(0);
        j[1] = pow(b, 1) / Factorial(1);
    }
    else
    {
        j[0] = sin(b) / b;
        j[1] = sin(b) / pow(b, 2) - cos(b) / b;
    }
    n[0] = -cos(b) / b;
    n[1] = -(cos(b) / pow(b, 2) + sin(b) / b);

    for (int k = 1; k < a; k++)
    {
        if (b <= 0.2)
        {
            j[k + 1] = pow(b, k + 1) / Factorial(k + 1);
        }
        else
        {
            j[k + 1] = (2 * k + 1) * j[k] / b - j[k - 1];
        }

        n[k + 1] = (2 * k + 1) * n[k] / b - n[k - 1];
    }
}

int main()
{
    fstream myFile;

    myFile.open("output.txt", ios::out);
    myFile.close();
    myFile.open("output.txt", ios::app);

    int num = 100, l;
    double xmin = 0.01, xmax = 15, delx, x;
    cout << "ENTER THE VALUE OF L" << endl;
    cin >> l;
    double j[l];
    double n[l];

    delx = (xmax - xmin) / double(num - 1);
    myFile << "x j" << l << "(x) n" << l << "(x) Abs" << endl;
    for (int q = 0; q < num; q++)
    {
        x = xmin + q * delx;
        bessel(l, x, j, n);
        myFile << x << " " << j[l] << " " << n[l] << " " << sqrt(pow(j[l], 2) + pow(n[l], 2)) << endl;
    }

    return 0;
}