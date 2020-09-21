import java.util.Random;

/**
 * Extend this abstract class for your game implementation
 * @author Azad
 *
 */
public abstract class Game 
{
	Agent agent[]; // Array containing all the agents, here we only consider two player games.
	String name = "A Generic Game"; //A name for the Game object, it will be changed by the actual game class extending it
	
	Random random = new Random();
	Agent winner = null; // The winning agent will be saved here after the game compeltes, if null the game is drawn.
	
	
	public Game(Agent a, Agent b) 
	{
		// TODO Auto-generated constructor stub
		agent = new Agent[2];
		agent[0] = a;
		agent[1] = b;
		
	}
	
	/**
	 * The actual game loop, each player takes turn.
	 * The first player is selected randomly
	 */
	public void play()
	{
		updateMessage("Starting " + name + " between "+ agent[0].name+ " and "+ agent[1].name+".");
		int turn = random.nextInt(2);
		
		//System.out.println(agent[turn].name+ " makes the first move.");
		initialize(false);
		
		while(!isFinished())
		{
			updateMessage(agent[turn].name+ "'s turn. ");
			agent[turn].makeMove(this);
			showGameState();
			
			turn = (turn+1)%2;
		}
		
		if (winner != null)
			updateMessage(winner.name+ " wins!!!");
		else	
			updateMessage("Game drawn!!");
		
	}
	
	@Override
	public String toString() {
		// TODO Auto-generated method stub
		String str = "";
		return str;
	}
	
	/**
	 * 
	 * @return Returns true if the game has finished. Also must update the winner member variable.
	 */
	abstract boolean isFinished();
	
	/**
	 * Initializes the game as needed. If the game starts with different initial configurations, it can be read from file.
	 * @param fromFile if true loads the initial state from file. 
	 */
	abstract void initialize(boolean fromFile);
	
	/**
	 * Prints the game state in console, or show it in the GUI
	 */
	abstract void showGameState();
	
	/**
	 * Shows game messages in console, or in the GUI
	 */
	abstract void updateMessage(String msg);
}
