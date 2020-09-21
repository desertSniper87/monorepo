
/**
 * Example MiniMax agent extending Agent class.
 * Here, for simplicity of understanding min and max functions are written separately. One single function can do the task. 
 * @author Azad
 *
 */
public class MinimaxTTTAgent extends Agent
{
	
	public MinimaxTTTAgent(String name) {
		super(name);
		// TODO Auto-generated constructor stub
	}

	@Override
	public void makeMove(Game game) {
		// TODO Auto-generated method stub
		
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		TickTackToe tttGame = (TickTackToe) game;
		CellValueTuple best = max(tttGame);
		if(best.col!=-1)
		{
			tttGame.board[best.row][best.col] = role;
		}
		
	}
	
	private CellValueTuple max(TickTackToe game)
	{	
		CellValueTuple maxCVT = new CellValueTuple();
		maxCVT.utility = -100;
		
		int winner = game.checkForWin();
		
		//terminal check
		if(winner == role)
		{
			maxCVT.utility = 1; //this agent wins
			return maxCVT;
		}
		else if(winner!=-1) 
		{
			maxCVT.utility = -1; //opponent wins
			return maxCVT;  
		}
		else if (game.isBoardFull())
		{
			maxCVT.utility = 0; //draw
			return maxCVT;  
		}
		
		for (int i = 0; i < 3; i++) 
		{
			for (int j = 0; j<3;j++)
			{
				if(game.board[i][j]!=-1) continue;
				
				game.board[i][j] = role; //temporarily making a move
				int v = min(game).utility;
				if(v>maxCVT.utility)
				{
					maxCVT.utility=v;
					maxCVT.row = i;
					maxCVT.col = j;
				}
				game.board[i][j] = -1; // reverting back to original state
				
			}
		}
		return maxCVT;
			
	}
	
	private CellValueTuple min(TickTackToe game)
	{	
		CellValueTuple minCVT = new CellValueTuple();
		minCVT.utility = 100;
		
		int winner = game.checkForWin();
		
		//terminal check
		if(winner == role)
		{
			minCVT.utility = 1; //max wins
			return minCVT;
		}
		else if(winner!=-1) 
		{
			minCVT.utility = -1; //min wins
			return minCVT;  
		}
		else if (game.isBoardFull())
		{
			minCVT.utility = 0; //draw
			return minCVT;  
		}
		
		for (int i = 0; i < 3; i++) 
		{
			for (int j = 0; j<3;j++)
			{
				if(game.board[i][j]!=-1) continue;
				
				game.board[i][j] = minRole();
				int v = max(game).utility;
				if(v<minCVT.utility)
				{
					minCVT.utility=v;
					minCVT.row = i;
					minCVT.col = j;
				}
				game.board[i][j] = -1;
				
			}
		}
		return minCVT;
			
	}
	
	private int minRole()
	{
		if(role==0)return 1;
		else return 0;
	}

	class CellValueTuple
	{
		int row,col, utility;
		public CellValueTuple() {
			// TODO Auto-generated constructor stub
			row =-1;
			col =-1;
		}
	}

}
