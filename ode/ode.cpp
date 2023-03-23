#include <bits/stdc++.h>
using namespace std;

int n = 4;
double h = 0.25, x = 1, y = 2, f, s1, s2, s3, s4;
vector<vector<double>> E, H, R;
fstream file;

double func(double a, double b)
{
    return (2 * b / a);
}
void euler()
{
    E[0][0] = x;
    E[0][1] = y;
    E[0][2] = func(E[0][0], E[0][1]);

    for (int i = 1; i <= n; i++)
    {
        E[i][0] = E[i - 1][0] + h;
        E[i][1] = E[i - 1][1] + h * E[i - 1][2];
        E[i][2] = func(E[i][0], E[i][1]);
    }
}
void heun()
{
    H[0][0] = x;
    H[0][1] = y;
    H[0][2] = func(H[0][0], H[0][1]);
    H[0][3] = H[0][1];
    H[0][4] = H[0][2];

    for (int i = 1; i <= n; i++)
    {
        H[i][0] = H[i - 1][0] + h;
        H[i][1] = H[i - 1][3] + h * H[i - 1][4];
        H[i][2] = func(H[i][0], H[i][1]);
        H[i][3] = H[i - 1][3] + (H[i - 1][4] + H[i][2]) * h / 2;
        H[i][4] = func(H[i][0], H[i][3]);
    }
}
void rk()
{
    R[0][0] = x;
    R[0][5] = y;

    for (int i = 1; i <= n; i++)
    {
        R[i][0] = R[i - 1][0] + h;
        R[i][1] = h * func(R[i - 1][0], R[i - 1][5]);
        R[i][2] = h * func(R[i - 1][0] + h / 2, R[i - 1][5] + R[i][1] / 2);
        R[i][3] = h * func(R[i - 1][0] + h / 2, R[i - 1][5] + R[i][2] / 2);
        R[i][4] = h * func(R[i - 1][0] + h, R[i - 1][5] + R[i][3]);
        R[i][5] = R[i - 1][5] + (R[i][1] + 2 * R[i][2] + 2 * R[i][3] + R[i][4]) / 6;
    }
}

int main()
{
    E.resize(n + 1, vector<double>(3));
    H.resize(n + 1, vector<double>(5));
    R.resize(n + 1, vector<double>(6));
    file.open("output.txt", ios::out);

    euler();

    file << "using euler \n";
    for (int i = 0; i <= n; i++)
    {
        file << E[i][0] << " " << E[i][1] << "\n";
    }
    file << "\n";

    heun();

    file << "using heun \n";
    for (int i = 0; i <= n; i++)
    {

        file << H[i][0] << " " << H[i][3] << "\n";
    }
    file << "\n";

    rk();

    file << "using runge-kutta \n";
    for (int i = 0; i <= n; i++)
    {
        file << R[i][0] << " " << R[i][5] << "\n";
    }

    return 0;
}