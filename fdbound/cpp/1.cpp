#include <bits/stdc++.h>
#include <Eigen/Dense>

using namespace Eigen;
using namespace std;

int main()
{

    int n = 100, lwork, info;
    double t, hbar = 1, m = 1, dx, l = 5, e, x[n], v[n];
    vector<vector<double>> evec(n);
    MatrixXd h(100, 100);
    MatrixXcd D, o;

    dx = 5 / real(n - 1);
    t = pow(hbar, 2) / (2 * m * pow(dx, 2));

    fstream file1, file2, file3;
    file1.open("hamiltonian.txt", ios::out);
    file2.open("eigenvalue.txt", ios::out);
    file3.open("eigenvector.txt", ios::out);

    for (int i = 0; i < n; i++)
    {
        x[i] = i * dx;
        v[i] = 0;
        for (int j = 0; j < n; j++)
        {
            if (i == j)
                h(i, j) = 2 * t + v[i];
            else if (abs(i - j) == 1)
                h(i, j) = -t;
            else
                h(i, j) = 0;
        }
    }

    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < n; j++)
        {
            file1 << h(i, j) << " ";
        }
        file1 << endl;
    }
    file1 << endl;

    EigenSolver<MatrixXd> es(h);

    D = es.eigenvalues();
    o = es.eigenvectors();

    for (int i = 0; i < n; i++)
    {
        evec[i].push_back(D(i, 0).real());
        for (int j = 0; j < n; j++)
        {
            evec[i].push_back(o(j, i).real());
        }
    }
    sort(evec.begin(), evec.end());
    for (int i = 0; i < n; i++)
    {
        e = pow((i + 1) * 2 * M_PI, 2) / (8 * m * pow(l, 2));
        file2 << i + 1 << " " << evec[i][0] << " " << e << endl;
        file3 << x[i] << " ";
        for (int j = 0; j < n; j++)
        {
            file3 << evec[j][i + 1] << " ";
        }
        file3 << endl;
    }

    return 0;
}
