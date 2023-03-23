#include <bits/stdc++.h>
using namespace std;

/* name of program */

 vector<double> x; 
 vector<vector<double>> q;

double product(int a, double b)
{
    double prod = 1;

    for (int k = 0; k < a; k++)
    {
        prod *= (b - x[k]);
    }
    return prod;
}

int main()
{
    int n;
    double m;
    
    cin >> n>>m ;
    // double x[n+1],q[n+1][n+1];



    for (int i = 0; i <= n; i++)
    {
        x.push_back(cos((2 * i + 1) * M_PI / (2 * n + 2)));
        q.push_back({sin(M_PI * x[i])});
    }

    for (int c = 1; c <= n; c++)
    {
        for (int r = 0; r <= n - c; r++)
        {
            q[r].push_back ((q[r + 1][c - 1] - q[r][c - 1]) / (x[r + c] - x[r]));
        }
    }

    for (int i=0;i<=n;i++){
        for(auto x : q[i]){
            cout<<x<<" ";
        }
        cout<<endl;
    }

    double ans = q[0][0];

    for (int i = 1; i <= n; i++)
    {
        ans += q[0][i] * product(i, m);
    }

    cout << ans << endl;

    return 0;
}