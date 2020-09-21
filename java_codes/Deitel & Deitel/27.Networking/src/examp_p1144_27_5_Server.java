import java.io.IOException;
import java.io.EOFException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamException;
import java.net.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

public class examp_p1144_27_5_Server extends JFrame 
{
	private JTextField enterField;
	private JTextArea displayArea;
	private ServerSocket server;
	private Socket connection;
	private ObjectInputStream input;
	private ObjectOutputStream output;
	private int counter;
	
	public examp_p1144_27_5_Server()
	{
		super ( "Server" );
		
		enterField = new JTextField ();
		enterField.setEditable(false);
		enterField.addActionListener(
				new ActionListener() {
					
					@Override
					public void actionPerformed(ActionEvent event)
					{
						// TODO Auto-generated method stub
						sendData ( event.getActionCommand() );
						enterField.setText("");
					}
				}
				);
		
		displayArea = new JTextArea ();
		add ( new JScrollPane( displayArea ) );
		
		setSize ( 150,300 );
		setVisible ( true );
	}
	public void runServer ()
	{
		try
		{
			server = new ServerSocket ( 9090,100 );
			while ( true )
			{
				try
				{
					waitForConnection();
					getStreams();
					processConnection();
				}	//end try
				catch ( EOFException eofException )
				{
					displayMessage ("\nServer terminated connection\n");
				}
				finally 
				{
					closeConnection ();
					++counter;
				}
			}	//end while
		}	//end try
		catch ( IOException ioException )
		{
			ioException.printStackTrace();
		}
	}
	public void waitForConnection() throws IOException
	{
		// TODO Auto-generated method stub
		
		displayMessage ("\nWaiting for connections\n");
		connection = server.accept();
		displayMessage ("Connection recieved from" + counter + connection.getInetAddress().getHostName());
		
	}
	public void getStreams() throws IOException
	{
		// TODO Auto-generated method stub
		
		output = new ObjectOutputStream( connection.getOutputStream() );
		output.flush();
		
		input = new ObjectInputStream ( connection.getInputStream() );
	}
	public void processConnection() throws IOException 
	{
		// TODO Auto-generated method stub
		String message = "Connection successful";
		sendData(message);
	
		enterField.setEditable( true );
		for ( ;message!="CLIENT>>> TERMINATE"; )
		{
			try
			{
				message = (String) input.readObject();
				displayMessage (message +  "\n");
			}
			catch ( ClassNotFoundException cnfe )
			{
				displayMessage ("\nUnknown object type received\n");
			}
		}
		
	}
	public void closeConnection () throws IOException
	{
		enterField.setEditable(false);
		
		input.close();
		output.close();
		connection.close();
	}
	
	public void sendData ( String message )
	{
		try
		{
			output.writeObject("Server>>> "+ message);
			output.flush();
			displayMessage("\nServer>>> "+ message);
		}
		catch ( IOException ioException ) 
		{
			displayArea.append("\nError writing object");
		}
	}
	private void displayMessage ( final String messageToDisplay )
	{
		SwingUtilities.invokeLater
		(
				new Runnable()
				{
					public void run()
					{
						displayArea.append (messageToDisplay);
					}
				}
		);
	}
	
	private void setTextFieldEditable ( final boolean editable )
	{
		SwingUtilities.invokeLater
		(
				new Runnable()
				{
					public void run ()
					{
						enterField.setEditable( editable );
					}
				}
		);
	}
	}
