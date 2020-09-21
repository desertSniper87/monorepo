#include<iostream>
#include<conio.h>
#include<stdlib.h>
using namespace std;
int cost[10][10],i,j,k,n,stk[10],top,v,visit[10],visited[10],u;

main()
{
	int m,c;
	cout <<"enterno of vertices";
	cin >> n;
	cout <<"ente no of edges";
	cin >> m;
	cout <<"\nEDGES Cost\n";
	for(k=1;k<=m;k++)
	{
		cin >>i>>j>>c;
		cost[i][j]=c;
	}
	for(i=1;i<=n;i++)
	for(j=1;j<=n;j++)
		if(cost[i][j]==0)
		cost[i][j]=31999;

	cout <<"ORDER OF VISITED VERTICES";
	k=1;
	while(k<n)
	{
		m=31999;
		if(k==1)
		{
			for(i=1;i<=n;i++)
				for(j=1;j<=m;j++)
				if(cost[i][j]<m)
				{
					m=cost[i][j];
					u=i;
				}
		}
		else
		{
			for(j=n;j>=1;j--)
			if(cost[v][j]<m && visited[j]!=1 && visit[j]!=1)
			{
				visit[j]=1;
				stk[top]=j;
				top++;
				m=cost[v][j];
				u=j;
			}
		}
		cost[v][u]=31999;
		v=u;
		cout<<v << " ";
		k++;
		visit[v]=0; visited[v]=1;
	}
}
