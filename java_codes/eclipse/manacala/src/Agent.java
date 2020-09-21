
/**
 * Extend this abstract class for human/ AI agent
 * @author Azad
 *
 */
public abstract class Agent 
{
	String name; // Name of the agent
	int role; // This is important, Each agent will be assigned a role beforehand. 
				//For example, for tick tack toe X will be assigned to one agent, and 0 will be assigned to another agent
				// The roles are stored as integer. 
	public Agent(String name) 
	{
		// TODO Auto-generated constructor stub
		this.name = name;
		
	}
	
	/**
	 * Sets the role of this agent. Typlically will be called by your extended Game class (The  class which extends the Game Class).
	 * @param role
	 */
	public void setRole(int role) {
		this.role = role;
	}

	/**
	 * Implement this method to select a move, and change the game state according to the chosen move.
	 * @param game
	 */
	public abstract void makeMove(Game game);
	

}
