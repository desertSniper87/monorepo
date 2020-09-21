
//package mancala;
            import java.applet.*;
            import java.awt.*;
            import java.util.*;
            import java.awt.event.*;


/*
 Hey Tony--remember that we want to let computers
 play computers and computers play humans.
 That means keep things general.
 Do you want to let humans play humans?  It
 might not but much more work, but I sort of
 doubt it's worth it.



 */




    public class Mancala extends Applet {


        boolean ABPRUNE=true;
        boolean RANDOM=true;

        String mode;
        Point nextMove;

        int RAD = 9;
        int BIN = 85;
        int LEFT = 20;
        int TOP = 180;
        int PAD = 7;
        int MARBLESPERBIN = 4;

        int HANDX = LEFT + 5 * BIN;
        int HANDY = TOP - BIN;
        int HANDSIZE = (int)(.5 * BIN);

        final int DEFDEPP = 7;

        final int PX = 1;//location of turn in those 2D arrays
        final int PY = 0;
        final int DX = 0;//location of depth in those 2D arrays
        final int DY = 7;
        final float IMPOSSIBLE = -50000000;
        int NOTDONE = -1234567;

        boolean gameOver = false;
        int gainAll;//number of player that takes everything
        boolean playing = false;
        boolean p0turn;

        Board b;
        MancalaComp main;

        int countDown;
        int maxCount;

        Graphics g;
        Graphics temp;
        Image tempIm;
        int h, v;
        int boardViewI = 0;

        int frames;
        int depth;
        int endTurnI;
        int endGameI;
        boolean humanFirst;



        Vector hand = new Vector();
        Vector all = new Vector();

        boolean waitForClick = false;

        public void init() {




            addMouseListener(new MouseControl()); //mouse control
            addMouseMotionListener(new MouseMove()); //mouse control 2
            addKeyListener(new KeyBoard());


            h = getWidth();
            v = getHeight();
            h = 900;//cheating
            v = 380;
            tempIm = createImage(h, v);
            temp = tempIm.getGraphics();
            g = getGraphics();

            DisplayScheme ddd = new DisplayScheme(new Color(50, 0, 0),
                    new Color(172, 170, 175), new Color(100, 100, 110),
                    new Color(34, 34, 200), 16, 4, 6);




            main = new MancalaComp(ddd);
            main.mainSet(0, 0, h, v, ddd);
            main.defaultOptions();
            b = new Board();
            setGame(false);

        }


        public void start() {
            //make a Thread and run it
            Thread runner = new Thread() {
                public void run() {
                    while (true) {



                        //System.out.println("mode: "+mode+" count: "+countDown+" max: "+maxCount+ "play"+playing);
                        //System.out.println("game over: "+gameOver+" p0turn: "+p0turn);
                        //System.out.println("                        ht");
                        if (countDown > 0) {
                            tick();
                        } else if (playing) {


                            if (!gameOver && hand.isEmpty() &&
                                    (mode == "grabbed"||mode == "moved") &&
                                    checkForWinner()) {
                                if (mode == "final tally") {
                                    countDown = maxCount = (101 - frames);

                                }
                            } else if (p0turn && !gameOver && mode == "moved") {
                                //System.out.println("                                   (H.T. line)");
                                nextMove = new Point(playInt(),
                                        bestMove(b.get2DArray()));
                                mode = "ready";
                                //grab(nextMove);
                                //countDown=maxCount=(101-frames);

                            } else if (!gameOver && mode == "ready") {
                                // System.out.println("                                   (H.T. line)");
                                //  nextMove=bestMove(b.get2DArray());
                                grab(nextMove);
                                mode = "grabbed";
                                countDown = maxCount = (101 - frames);
                            } else if (!gameOver && mode == "grabbed") {


                                if (hand.isEmpty()) {

                                    mode = "moved";


                                    if ((p0turn && nextMove.y >= 0) ||
                                            (!p0turn && nextMove.y <= 5)) {//landed in last bowl, don't switch turns


                                        if (addBin(nextMove)) {
                                            countDown = maxCount = (101 - frames);
                                        }


                                        p0turn = !p0turn;
                                    } //else {
                                    //checkForWinner();
                                    //}

                                } else {
                                    moveOne(nextMove);
                                    countDown = maxCount = (101 - frames);
                                }

                            } else if (gameOver && mode == "final tally") {
                                moveAll();
                                countDown = maxCount = (101 - frames);
                                mode = "winner";
                            } else if (gameOver && mode == "winner") {
                                if (b.p0bin.size() > b.p1bin.size()) {
                                    main.announce("Game Over","Tim 'n Tony win!");
                                } else if (b.p0bin.size() < b.p1bin.size()) {
                                    main.announce("Game Over","You won! (just luck)");

                                } else {
                                    main.announce("Game Over","Tie Game--not necktie game!");
                                }

                                mode = "done";

                            }

                        }
                        paint(g);
                    }
                }
            };

            runner.start();
        }

        public int playInt() {
            if (p0turn) {
                return 0;
            } else {
                return 1;
            }
        }

        public void tick() {


            countDown--;

            if (countDown <= 0) {
                for (int i = 0; i < all.size(); i++) {
                    Marble m = ((Marble) all.get(i));
                    m.oldX = m.x;
                    m.oldY = m.y;
                }

            }
        }


        public void paint(Graphics g) {


            temp.setColor(Color.black);//in case the image doesn't load
            temp.fillRect(0, 0, h, v);


		/*
		 moveAllRec(first,countDown,maxMove);

		 if(first!=null){
		 first.paintLines(temp);//paints the rest recursively
		 first.paintSelf(temp);
		 }

		 int gap=2;
		 Color r=Color.red;
		 Color gg=Color.green;
		 Color b=Color.blue;
		 Color nodeC=Color.white;

		 paintAt(temp, start, 125, 30, gap,12,r, gg, b,nodeC,false,false,0,0);
		 paintAt(temp, goal,  203, 30, gap,12,r, gg, b,nodeC,false,false,0,0);
		 */

            main.mainPaint(temp);


            g.drawImage(tempIm, 0, 0, null);



        }


        public int checkBoardForWinner(int[][] board) {
            //-123456 --game continues //NOTDONE
            //-1 --tie
            //0--p0 wins
            //1 --p1 wins
            boolean p0empty = true;
            boolean p1empty = true;
            for (int j = 0; j < 6; j++) {
                if (board[0][j + 1] != 0) {
                    p0empty = false;
                }
                if (board[1][j + 1] != 0) {
                    p1empty = false;
                }

            }

            if (!p0empty && !p1empty) {
                return NOTDONE;
            }



            int[] bins = new int[2];
            bins[0] = board[0][0];
            bins[1] = board[1][7];

            if (endGameI != 1) {
                if (p0empty) {
                    gainAll = 0; //convert empty to an int
                } else {
                    gainAll = 1;
                }
                int marblesLeft = MARBLESPERBIN * 12 - bins[0] - bins[1];
                if (endGameI == 2) {//switch it if the person that wins doesn't get everything
                    gainAll = 1 - gainAll;
                }
                bins[gainAll] += marblesLeft;
            }
            if (bins[0] == bins[1]) {
                return 0;
            } else if (bins[0] > bins[1]) {
                return 90000 + 50 * (bins[0] - bins[1]) + board[DX][DY];
                //return 0;
            } else {
                return -90000 + 50 * (bins[0] - bins[1]) - board[DX][DY];
                //return 1;
            }
        }





        public boolean checkForWinner() {

            boolean p0empty = true;
            boolean p1empty = true;
            for (int j = 0; j < 6; j++) {
                if (!b.theBins[0][j].isEmpty()) {
                    p0empty = false;
                }
                if (!b.theBins[1][j].isEmpty()) {
                    p1empty = false;
                }

            }

            if (!p0empty && !p1empty) {
                return false;
            }

            gameOver = true;
            mode = "winner";

            if (endGameI == 1 || (p1empty && p0empty)) {
                return true;
            }

            if (p0empty) {
                gainAll = 0; //convert empty to an int
            } else {
                gainAll = 1;
            }

            //use the int
            hand = new Vector();
            for (int j = 0; j < 6; j++) {
                Vector v = b.theBins[1 - gainAll][j];

                while (!v.isEmpty()) {
                    Marble m = (Marble) v.remove(v.size() - 1);
                    m.set(HANDX, HANDY, hand, HANDSIZE);
                    hand.add(m);
                }
            }




            if (endGameI == 2) {//switch it if the person that wins doesn't get everything
                gainAll = 1 - gainAll;
            }


            mode = "final tally";
            return true;
        }
	/*
	public void checkForWinner() {
		int player = 1;
		if (p0turn) {
			player = 0;
		}
		int j;
		for (j = 0; j < 6; j++) {
			if (!b.theBins[player][j].isEmpty())
				return;
		}

		int[] p = new int[2];
		p[0] = b.p0bin.size();
		p[1] = b.p1bin.size();

		if (endGameI == 0) {
			for (j = 0; j < 6; j++) {
				p[player] += b.theBins[1 - player][j].size();
			}
		} else if (endGameI == 2 || endGameI == 3) {
			for (j = 0; j < 6; j++) {
				p[1 - player] += b.theBins[1 - player][j].size();
			}

		}

		if (p[0] > p[1]) {
			main.announce("Game Over","Tim 'n Tony win!");
		} else if (p[0] < p[1]) {
			main.announce("Game Over","You won! (you just got lucky)");

		} else {
			main.announce("Game Over","Tie Game--but not necktie game!");
		}

		gameOver = true;




}*/

        public void grab(Point p) {
            int i = p.x;
            int j = p.y;

            Vector v = b.theBins[i][j];

            hand = new Vector();
            //System.out.println("handSize: "+hand.size()+ " v size: "+v.size());
            //b.theBins[i][j]=new Vector();
            while (!v.isEmpty()) {
                Marble m = (Marble) v.remove(v.size() - 1);
                m.set(HANDX, HANDY, hand, HANDSIZE);
                hand.add(m);
            }
            //System.out.println("handSize: "+hand.size()+ " v size: "+v.size());

        }


        public void moveAll() {


            int size;
            int xx, yy;

            Vector v2;

            if (gainAll == 0) {
                v2 = b.p0bin;
                size = BIN - RAD - 2 * PAD;
                xx = LEFT + BIN;
                yy = TOP + BIN;
            } else {
                v2 = b.p1bin;
                size = BIN - RAD - 2 * PAD;
                xx = LEFT + BIN * 9;
                yy = TOP + BIN;
            }





            while (!hand.isEmpty()) {
                Marble m = (Marble) hand.remove(hand.size() - 1);
                m.set(xx, yy, v2, size);
                v2.add(m);
            }



            //((Marble) o).set(xx, yy, v2, size);
            //v2.add(o);
            // }

            //System.out.println("daft: "+i + " "+j + " "+b.p1bin.size());


        }

        public void moveOne(Point p) {

            int player = playInt();
            int j = p.y;

            int i = p.x;

            Vector v = hand;

            if (v.isEmpty()) {
                System.out.println();
                System.out.println("player: "+player + " j: "+j);
                System.out.println("Can't move with 0");
                return;
            }

            Vector v2;

            // while (!v.isEmpty()) {

            nextMove = next(i, j, player);
            i = nextMove.x;
            j = nextMove.y;


            Object o = v.remove(v.size() - 1);

            int size;
            int xx, yy;

            if (j < 0) {
                v2 = b.p0bin;
                size = BIN - RAD - 2 * PAD;
                xx = LEFT + BIN;
                yy = TOP + BIN;
            } else if (j > 5) {
                v2 = b.p1bin;
                size = BIN - RAD - 2 * PAD;
                xx = LEFT + BIN * 9;
                yy = TOP + BIN;
            } else {
                v2 = b.theBins[i][j];
                size = BIN / 2 - RAD - PAD;
                xx = LEFT + 2 * BIN + j * BIN + BIN / 2;
                yy = TOP + i * BIN + BIN / 2;
            }
            //System.out.println("bef: "+i + " "+j + " "+v2.size());

            //System.out.println("aft: "+i + " "+j + " "+v2.size());
            ((Marble) o).set(xx, yy, v2, size);
            v2.add(o);
            // }

            //System.out.println("daft: "+i + " "+j + " "+b.p1bin.size());

        }

        public boolean addBin(Point nextMove) {
            int i = nextMove.x;
            int j = nextMove.y;


            Vector v2 = b.theBins[i][j];
            if (v2.size() == 1) {//if only this shell in the last bin
                if (endTurnI != 2) {
                    if (endTurnI == 0 || i == playInt()) {
                        Vector v3;
                        if (playInt() == 0) {
                            v3 = b.p0bin;
                        } else {
                            v3 = b.p1bin;
                        }


                        Vector v4 = b.theBins[1 - i][j];
                        for (int k = 0; k < v4.size(); k++) {
                            ((Marble) v4.get(k)).set(LEFT + BIN + 8 * BIN * playInt(),
                                    TOP + BIN, v3, BIN - RAD - PAD);
                            v3.add(v4.get(k));
                        }

                        if (endTurnI == 3 && b.theBins[1 - i][j].size() > 0) {//Snails!
                            for (int k = 0; k < v2.size(); k++) {
                                ((Marble) v2.get(k)).set(LEFT + BIN + 8 * BIN * playInt(),
                                        TOP + BIN, v3, BIN - RAD - PAD);
                                v3.add(v2.get(k));
                            }

                            v2.clear();
                        }
                        v4.clear();
                        return true;
                    }
                }

            }

            return false;
        }


        public Point next(int i, int j, int player) {
            //int nexti=i;
            //int nextj=j;

            //System.out.println("next of "+i + " "+j + " with player "+player + " ....");


            if (i == 0) {
                j--;
            } else {
                j++;
            }

            if ((j <= -1 && player == 1) || j <= -2) {
                j = 0;
                i = 1;
            }

            if ((j >= 6 && player == 0) || j >= 7) {
                j = 5;
                i = 0;
            }

            //	System.out.println("...is "+i + " "+j);

            return new Point(i, j);


        }

        public int[][] copy(int[][] a) {
            int[][] ret = new int[2][8];
            for (int i = 0; i < 2; i++) {
                for (int j = 0; j < 8; j++) {
                    ret[i][j] = a[i][j];

                }
            }
            return ret;
        }


        public float h_stic(int[][] board) {



            float ret = 0;
            ret = board[0][0] - board[1][7];







            //ret /= (20 + shellsLeft);
            return ret;

        }

        public void move(int j, int[][] board) {


            int player = board[PX][PY];
            int i = player;
            int v = board[i][j + 1];
            board[i][j + 1] = 0;
            if (v == 0) {
                System.out.println("We screwed something up royally");
                return;
            }
            while (v > 0) {
                Point p = next(i, j, player);
                i = p.x;
                j = p.y;
                v--;
                board[i][j + 1]++;
            }

            board[DX][DY]--;
            //System.out.println("i:"+i+" j: "+j+" player: "+player);
            if (j + 1 == 7 * player) {//landed in last bowl, don't switch turns
                return;
            }



            board[PX][PY] = 1 - board[PX][PY];

            if (board[i][j + 1] == 1) {//if only this shell in the last bin
                if (endTurnI != 2) {
                    if (endTurnI == 0 || i == player) {
                        if (endTurnI == 3 && board[1 - i][j + 1] != 0) {//snails!
                            board[player][7 * player] += board[i][j + 1];
                            board[i][j + 1] = 0;
                        }
                        board[player][7 * player] += board[1 - i][j + 1];
                        board[1 - i][j + 1] = 0;



                    }
                }
            }
            //System.out.println("Switching player: "+board[PX][PY]);

            //System.out.println("done: "+board[PX][PY]);
        }

        public void printBoard(int[][] board) {
            System.out.println();

            System.out.println("Board: ");
            for (int i = 0; i < 2; i++) {
                for (int j = 0; j < 8; j++) {
                    System.out.print(board[i][j] + " ");
                }
                System.out.println();

            }

        }

        public float minMax(int m, int[][] board, Interval mine, Interval dads) {

            int player = board[PX][PY];

            if (board[player][m + 1] == 0) {
                return IMPOSSIBLE;
            }
            move(m, board);
            int win = checkBoardForWinner(board);
            if (win != NOTDONE) {
                //System.out.println("Detected a win!");
                //if (win == 0) {
                //	return 10000;
                //}else if(win == 1){
                //	return -10000;
                //}else{
                //	return 0;
                //}
                return win;
            }

            boolean change = (board[PX][PY] == player);
            player = board[PX][PY];

            Interval next1;
            Interval next2;
            if (change) {
                next1=new Interval();
                next2 = mine;
            } else {
                next1 = mine;
                next2 = dads;
            }

            int depth = board[DX][DY];
            if (depth == 0) {
                return h_stic(board);
            }
            if (player == 1) {
                //if (change) {
                //	next1 = new Interval();
                //}
                float minSoFar = minMax(0, copy(board), next1, next2);
                for (int i = 1; i < 6; i++) {
                    if (change) {
                        next1 = new Interval();
                    }
                    float f = minMax(i, copy(board), next1, next2);
                    if(ABPRUNE&&dads.min>f&&f>IMPOSSIBLE){
                        //System.out.println("min Prune");
                        return f;
                    }

                    if (mine.max > f&&f>IMPOSSIBLE) {
                        mine.max = f;
                    }

                    if ((f < minSoFar && f > 100 + IMPOSSIBLE) ||
                            minSoFar <= IMPOSSIBLE + 10) {
                        minSoFar = f;
                    }
                }
                if (minSoFar == IMPOSSIBLE) {
                    System.out.println("---------minscrew:-------");
                    System.out.println("win is "+checkBoardForWinner(board));
                    printBoard(board);

                }

                return minSoFar;
            } else {
                //if (change) {
                ///	next1 = new Interval();
                //}
                float maxSoFar = minMax(0, copy(board), next1, next2);
                for (int i = 1; i < 6; i++) {
                    if (change) {
                        next1 = new Interval();
                    }
                    float f = minMax(i, copy(board), next1, next2);
                    if(ABPRUNE&&dads.max<f&&f>IMPOSSIBLE){
                        //System.out.println("max Prune");
                        return f;
                    }
                    if (mine.min < f&&f>IMPOSSIBLE) {
                        mine.min = f;
                    }


                    if (f > maxSoFar) {
                        maxSoFar = f;
                    }
                }
                if (maxSoFar == IMPOSSIBLE) {
                    System.out.println("---------maximumscrew:-------");
                    System.out.println("win is "+checkBoardForWinner(board));
                    printBoard(board);

                }
                return maxSoFar;
            }
        }


	/*
	public int worstMove(int[][] board) {
		int worstMove = 0;
		float maxSoFar = minMax(0, copy(board), new Interval(), new Interval());
		//System.out.println("Move "+0+"has value "+maxSoFar);
		for (int i = 1; i < 6; i++) {
			float f = minMax(i, copy(board), new Interval(),new Interval());
			//System.out.println("Move "+i+"has value "+f);
			if (f > maxSoFar || (f == maxSoFar && (RANDOM && Math.random() < .5))) {
				maxSoFar = f;
				bestMove = i;
			}

		}
		if (maxSoFar == IMPOSSIBLE) {
			System.out.println("We screwed up really bad");
			for (int i = 1; i < 6; i++) {
				if (b.theBins[0][i].size() > 0) {
					return i;
				}
			}
		}
		return bestMove;
	}*/


        public int bestMove(int[][] board) {

            //  printBoard(board);

            int bestMove = 0;

            float maxSoFar = minMax(0, copy(board), new Interval(), new Interval());
            //System.out.println("Move "+0+"has value "+maxSoFar);
            for (int i = 1; i < 6; i++) {
                float f = minMax(i, copy(board), new Interval(),new Interval());
                //System.out.println("Move "+i+"has value "+f);
                if (f > maxSoFar || (f == maxSoFar && (RANDOM && Math.random() < .5))) {
                    maxSoFar = f;
                    bestMove = i;

                }

            }

            if (maxSoFar == IMPOSSIBLE) {

                System.out.println("We screwed up really bad");
                for (int i = 1; i < 6; i++) {
                    if (b.theBins[0][i].size() > 0) {
                        return i;
                    }
                }
            }



            return bestMove;
        }

        public void setGame(boolean bb) {

            p0turn = !humanFirst && bb;

            if (bb) {
                countDown = maxCount = 101 - frames;
                gameOver = false;
                main.purge();
                mode = "moved";
            }

            b.setGame(bb);
            playing = bb;
            //countDown = 0;


            main.menu.setDisable("Start",bb);
            main.menu.setDisable("End Game",!bb);
            main.menu.setDisable("Default Options",bb);
            main.menu.setDisable("AutoMove",!bb);
            main.menu.setDisable("AI Depth...",bb);
            main.menu.setDisable("First Move...",bb);
            main.menu.setDisable("End of Turn...",bb);
            main.menu.setDisable("End of Game...",bb);
            main.menu.setDisable("Start Shells...",bb);


        }


        class Interval {
            float min, max;

            public Interval() {
                min = -190000;//less than lose
                max = 190000;//more than a win

            }

        }

        class MancalaComp extends Window {//GUI component


            //MenuBar theMenu;
            ScrollWindow speed;
            DropDownWindow endTurn;
            DropDownWindow endGame;
            DropDownWindow aiDepth;
            DropDownWindow firstMove;
            DropDownWindow startShells;
            DropDownWindow boardView;




            public MancalaComp(DisplayScheme _d) {

                title = "Mancala";
                setTopBar(false);

                height = v;
                width = h;


                d = _d;
                Vector v;
                //addComp(menu=new MenuBar());
                menu = new MenuBar();

                speed = new ScrollWindow("Animation speed...", this, 1, 100, 2, 0);

                //speed.currVal=30;
                // frames=30;
                // speed.setBeadLoc();

                v = new Vector();
                v.add("If last shell lands empty");
                v.add("Lands empty on home row");
                v.add("Never");
                v.add("Snails!");
                endTurn = new DropDownWindow("Take adjacent pits...", this, v);

                v = new Vector();
                v.add("First out");
                v.add("No one");
                v.add("Last out");

                endGame = new DropDownWindow("At end of game shells go to...", this, v);

                v = new Vector();
                v.add("Psychedelic Defeat");
                v.add("Boring Board");

                boardView = new DropDownWindow("The boad you lose on...", this, v);


                v = new Vector();
                v.add("One");
                v.add("Two");
                v.add("Three");
                v.add("Four");
                v.add("Five");
                v.add("Six");
                v.add("Seven");
                v.add("Eight");
                v.add("Nine");
                v.add("Ten");
                startShells = new DropDownWindow("Starting marbles per bin...", this, v);



                v = new Vector();
                v.add("First");
                v.add("Second");
                firstMove = new DropDownWindow("Human Moves...", this, v);

                v = new Vector();
                v.add("One");
                v.add("Two");
                v.add("Three");
                v.add("Four");
                v.add("Five");
                v.add("Six");
                v.add("Seven");
                v.add("Eight");
                v.add("Nine");
                aiDepth = new DropDownWindow("AI search depth.", this, v);

			/*	v = new Vector();
				v.add("1");
				v.add("2");
				v.add("3");
				v.add("4");
				v.add("5");
				startShells = new DropDownWindow("AI search depth.", this, v);*/


                v = new Vector();
                v.add("Start");
                v.add("End Game");
                v.add("About");
                menu.addMenu("Mancala",v);


                v = new Vector();
                v.add("Default Options");
                v.add("Close Windows");
                v.add("No Animation");
                v.add("AutoMove");
                menu.addMenu("Commands",v);

                v = new Vector();
                v.add("AI Depth...");
                v.add("First Move...");
                v.add("Speed...");
                v.add("End of Turn...");
                v.add("End of Game...");
                v.add("Start Shells...");
                menu.addMenu("Options",v);



                v = new Vector();
                v.add("Board...");
                v.add("Default");
                v.add("Green");
                v.add("Red");
                v.add("Gray");
                v.add("Blue");
                v.add("Black");
                v.add("Small");
                v.add("Medium");
                v.add("Large");
                menu.addMenu("View",v);

                init(null);

            }

            public void set() {
			/* choice.mainSet(340, 10, 150, 0, d);
			 choice2.mainSet(575, 10, 130, 0, d);
			 goBut.mainSet(730,10,50,0,d);
			 pauseBut.mainSet(730,44,50,0,d);
			 theDetail.mainSet(360,44,160,0,d);
			 theCheckBox.mainSet(690,44,0,0,d);
			 stepBut.mainSet(580,44,65,0,d);*/


                speed.mainSet(d);//since these are not in subComps, call explicitly
                endTurn.mainSet(d);
                endGame.mainSet(d);
                aiDepth.mainSet(d);
                firstMove.mainSet(d);
                startShells.mainSet(d);
                boardView.mainSet(d);

                //System.out.println("30 detail");
                // System.out.println("@detail "+detail+" curr "+theDetail.currVal);

            }


            public boolean mouse(int mx, int my, boolean pressed, boolean released) {


                b.mx = mx;
                b.my = my;





                if (menu.checkAndClearAction()) {



                    int mmenu = menu.currMenu;
                    int option = menu.getOption();


                    if (mmenu == 0) {//Mancala
                        if (option == 0) {//start
                            setGame(true);
                        } else if (option == 1) {//quit
                            setGame(false);
                        } else if (option == 2) {//about
                            //purge();
                            announce("About", "Made by Tim 'n Tony");
                        }



                    } else if (mmenu == 1) {//Commands

                        if (option == 0) {//Default

                            defaultOptions();



                            //addComp(newBuddy);
                        } else if (option == 1) {//close windows
                            purge();
                        } else if (option == 2) {//no animation
                            speed.main.currVal = 100;
                            frames = 100;
                            speed.main.setBeadLoc();
                        } else if (option == 3) {//automove
                            //move(bestMove(b.get2DArray()));

                            nextMove = new Point(1, bestMove(b.get2DArray()));
                            mode = "ready";
                        }



                    } else if (mmenu == 2) {//options
                        if (option == 0) {//aidepth



                            addComp(aiDepth);
                            moveToFront(aiDepth);


                        } else if (option == 1) {//firstMove
                            addComp(firstMove);
                            moveToFront(firstMove);
                        } else if (option == 2) {//Speed
                            addComp(speed);
                            moveToFront(speed);

                        } else if (option == 3) {//endTurn
                            addComp(endTurn);
                            moveToFront(endTurn);

                        } else if (option == 4) {//endGame
                            addComp(endGame);
                            moveToFront(endGame);


                        } else if (option == 5) {//startshells
                            addComp(startShells);
                            moveToFront(startShells);
                        }
                    } else if (mmenu == 3) {//view
                        if (option == 0) {
                            addComp(boardView);
                            moveToFront(boardView);
                        } else {
                            changeView(option);
                        }
                    }



                }



                MARBLESPERBIN = startShells.getVal() + 1;
                depth = aiDepth.getVal() + 1;
                endTurnI = endTurn.getVal();
                endGameI = endGame.getVal();
                humanFirst = (firstMove.getVal() == 0);
                boardViewI = boardView.getVal();


                if (frames != (int) speed.main.currVal) {
                    //int oldFrames=(int) speed.main.currVal;

                    countDown = (countDown * (101 - (int) speed.main.currVal)) /
                            (101 - frames);
                    maxCount = (maxCount * (101 - (int) speed.main.currVal)) /
                            (101 - frames);
                    frames = (int) speed.main.currVal;
                }


                return true;
            }

            //public void tick(){
            //detail=(int)theDetail.currVal;
            //}


            public void defaultOptions() {
                depth = DEFDEPP;
                endTurnI = 0;
                endGameI = 0;
                humanFirst = true;
                MARBLESPERBIN = 4;

                endTurn.main.setVal(0);
                endGame.main.setVal(0);
                aiDepth.main.setVal(depth - 1);
                firstMove.main.setVal(0);
                startShells.main.setVal(MARBLESPERBIN - 1);

                speed.main.currVal = 95;
                frames = 95;
                speed.main.setBeadLoc();

            }

            public void changeView(int n) {
                d = d.copy();
                if (n == 0) {//default
                    d.butC = new Color(100, 100, 110);
                    d.lineC = new Color(50, 0, 0);
                    d.bgC = new Color(172, 170, 175);
                    d.selC = new Color(34, 34, 200);
                } else if (n == 1) {//green
                    d.butC = new Color(70, 100, 70);
                    d.lineC = new Color(0, 20, 0);
                    d.bgC = new Color(150, 160, 150);
                    d.selC = new Color(0, 128, 0);
                } else if (n == 2 || n == 0) {//red
                    d.butC = new Color(160, 100, 100);
                    d.lineC = new Color(60, 0, 0);
                    d.bgC = new Color(170, 150, 150);
                    d.selC = new Color(255, 0, 0);
                } else if (n == 3) {//gray
                    d.butC = new Color(90, 90, 90);
                    d.lineC = new Color(14, 14, 14);
                    d.bgC = new Color(200, 200, 200);
                    d.selC = new Color(123, 123, 123);
                } else if (n == 4) {//blue
                    d.butC = new Color(100, 100, 160);
                    d.lineC = new Color(0, 0, 0);
                    d.bgC = new Color(150, 150, 160);
                    d.selC = new Color(0, 0, 255);
                } else if (n == 5) {//black
                    d.butC = new Color(0, 0, 0);
                    d.lineC = new Color(255, 0, 0);
                    d.bgC = new Color(55, 55, 55);
                    d.selC = new Color(0, 255, 0);
                }

                if (n == 6) {
                    d.pad = 2;
                    d.txt = 9;
                    d.bdr = 1;
                    d.init();
                } else if (n == 7) {
                    d.pad = 4;
                    d.txt = 12;
                    d.bdr = 2;
                    d.init();
                } else if (n == 8) {
                    d.pad = 8;
                    d.txt = 20;
                    d.bdr = 4;
                    d.init();
                }

                mainSet(x, y, width, height, d);

            }


            public void paint(Graphics g) {

                b.paint(temp);

            }


            public void announce(String title, String s) {


                AnnounceBox ab = new AnnounceBox(title, s, this);
                ab.mainSet(300 + (int)(200 * Math.random()),
                        50 + (int)(70 * Math.random()), 250, 0, d);
                addComp(ab);

            }



        }


        class Board {//the entire board


            Image board;

            int mx, my;

            Vector[][] theBins = new Vector[2][6];//the bins themselves
            Vector p0bin = new Vector();
            Vector p1bin = new Vector();

            public Board() {

                java.net.URL base = getDocumentBase();
                board = getImage(base, "board.gif");
                for (int i = 0; i < 2; i++) {
                    for (int j = 0; j < 6; j++) {
                        theBins[i][j] = new Vector();

                    }

                }

            }


            public void setGame(boolean b) {
                p0bin.clear();
                p1bin.clear();
                all.clear();
                for (int i = 0; i < 2; i++) {
                    for (int j = 0; j < 6; j++) {
                        theBins[i][j].clear();
                        if (b) {
                            for (int k = 0; k < MARBLESPERBIN; k++) {
                                Marble m = new Marble();

                                m.set(LEFT + 5 * BIN, TOP, all, 5 * BIN);
                                //m.set(LEFT+5*BIN, TOP-2*BIN, new Vector(), 0);

                                m.oldX = m.x;
                                m.oldY = m.y;
                                m.set(LEFT + 2 * BIN + BIN * j + BIN / 2,
                                        TOP + BIN * i + BIN / 2, theBins[i][j], BIN / 2 - RAD - PAD);
                                theBins[i][j].add(m);

                                all.add(m);

                            }
                        }
                    }

                }
                countDown = maxCount = (101 - frames);

            }


            public int[][] get2DArray() {
                int[][] ret = new int[2][8];

                for (int i = 0; i < 2; i++) {
                    for (int j = 0; j < 6; j++) {
                        ret[i][j + 1] = theBins[i][j].size();

                    }

                }

                ret[0][0] = p0bin.size();
                ret[1][7] = p1bin.size();
                ret[DX][DY] = depth;
                int player = 1;
                if (p0turn) {
                    player = 0;
                }
                ret[PX][PY] = player;

                return ret;
            }


            public void paint(Graphics g) {

                int n;
                Vector v;



                if (boardViewI == 0) {
                    g.drawImage(board, LEFT - 16, TOP - 30, null);
                } else {
                    g.setColor(new Color(200, 150, 100));
                    g.fillRect(LEFT, TOP, 10 * BIN, 2 * BIN);
                }






                for (int i = 0; i < 2; i++) {
                    for (int j = 0; j < 6; j++) {

                        g.setColor(new Color(150, 100, 50));
                        if (!p0turn && playing && !gameOver) {
                            if (mx >= LEFT + 2 * BIN + j * BIN + PAD &&
                                    mx <= LEFT + 2 * BIN + j * BIN + BIN - 2 * PAD &&
                                    my >= TOP + i * BIN + PAD &&
                                    my <= TOP + i * BIN + BIN - 2 * PAD) {
                                if (i == 1 && !theBins[i][j].isEmpty()) {
                                    if (boardViewI == 0) {

                                        g.setColor(new Color(0, 250, 125));
                                        g.drawOval(LEFT + 2 * BIN + j * BIN + PAD, TOP + i * BIN + 2,
                                                BIN - 2 * PAD, BIN - 2 * PAD);
                                        g.drawOval(LEFT + 2 * BIN + j * BIN + BIN / 4,
                                                TOP + i * BIN + BIN / 4, BIN / 2, BIN / 2);
                                    } else {
                                        g.setColor(new Color(100, 75, 25));

                                    }

                                }

                            }
                        }
                        if (boardViewI == 1) {
                            g.fillRect(LEFT + 2 * BIN + j * BIN + PAD, TOP + i * BIN + 2,
                                    BIN - 2 * PAD, BIN - 2 * PAD);
                        }

                    }
                }



                g.setColor(new Color(100, 250, 50));
                if (!p0turn && playing && !gameOver) {
                    g.drawString("Your turn!",100, TOP - 3);
                }





                if (boardViewI == 1) {
                    g.setColor(new Color(150, 100, 50));
                    g.fillRect(LEFT + PAD, TOP + PAD, 2 * BIN - 2 * PAD, 2 * BIN - 2 * PAD);


                    g.setColor(new Color(150, 100, 50));
                    g.fillRect(LEFT + PAD + BIN * 8, TOP + PAD, 2 * BIN - 2 * PAD,
                            2 * BIN - 2 * PAD);
                }


                for (int i = 0; i < 2; i++) {
                    for (int j = 0; j < 6; j++) {
                        v = theBins[i][j];
                        for (int k = 0; k < v.size(); k++) {
                            ((Marble) v.get(k)).paint(g);
                        }
                        if (playing) {
                            g.setColor(Color.white);

                            n = theBins[i][j].size();
                            g.drawString(""+n, LEFT + 2 * BIN + j * BIN + BIN / 2 + 1,
                                    TOP + i * BIN + BIN / 2 + 6);
                        }
                    }
                }
                v = p0bin;
                for (int i = 0; i < v.size(); i++) {
                    ((Marble) v.get(i)).paint(g);
                }
                g.setColor(new Color(0, 200, 255));


                v = p1bin;
                for (int i = 0; i < v.size(); i++) {
                    ((Marble) v.get(i)).paint(g);
                }


                if (playing) {
                    g.setColor(Color.white);

                    n = p1bin.size();

                    g.drawString(""+n, LEFT + BIN + 1 + BIN * 8, TOP + BIN + 6);
                    n = p0bin.size();
                    g.drawString(""+n, LEFT + BIN + 1, TOP + BIN + 6);

                }
                for (int k = 0; k < hand.size(); k++) {
                    Marble m = (Marble) hand.get(k);
                    m.paint(g);
                }

            }





            public void mouse(int mx, int my) {
                if (p0turn || gameOver) {
                    return;
                }
                int i = 1;
                for (int j = 0; j < 6; j++) {
                    if (mx >= LEFT + 2 * BIN + j * BIN + PAD &&
                            mx <= LEFT + 2 * BIN + j * BIN + BIN - 2 * PAD &&
                            my >= TOP + i * BIN + PAD &&
                            my <= TOP + i * BIN + BIN - 2 * PAD) {
                        if (!b.theBins[i][j].isEmpty()) {
                            mode = "ready";
                            nextMove = new Point(1, j);
                        }
                        return;
                    }
                }







            }



        }




        class Marble {
            public int x, y;//where the marble is drawn (center)

            public int oldX, oldY;

            public int rad;//for graphics
            public Color c;
            public Marble() {

                rad = RAD;

                double d = Math.random() * Math.PI * 2;
                int red = (int)(128 * (1 + Math.cos(d + Math.PI * 0.0 / 3.0)));
                int green = (int)(128 * (1 + Math.cos(d + Math.PI * 2.0 / 3.0)));
                int blue = (int)(128 * (1 + Math.cos(d + Math.PI * 4.0 / 3.0)));
                c = new Color(red, green, blue);

            }


            /*public void set(int i, int j, Vector v, int size) {
             x = i + (int)(Math.random() * size * 2) - size;
             y = j + (int)(Math.random() * size * 2) - size;
             }
             */
            public void set(int cx, int cy, Vector v, int size) {

                oldX = x;
                oldY = y;
                int dist;
                boolean works;
                int tries;
                for (tries = 0; tries <= 200; tries++) {

                    x = (int)(cx - size + (Math.random() * 2 * size));
                    y = (int)(cy - size + (Math.random() * 2 * size));
                    dist = (int)(4 * RAD * (200 - tries)) / 200;
                    works = true;

                    if (dist(x, y, cx, cy) > 1.1 * size) {
                        works = false;
                        continue;
                    }

                    for (int i = 0; i < v.size(); i++) {
                        Marble m = (Marble) v.get(i);
                        if (dist(x, y, m.x, m.y) < dist) {
                            works = false;
                            break;
                        }
                    }
                    if (works) {
                        break;
                    }


                }

                if (tries >= 201) {
                    x = (int)(cx - size + (Math.random() * 2 * size));
                    y = (int)(cy - size + (Math.random() * 2 * size));

                }

            }

            public double dist(int x, int y, int xx, int yy) {
                int dx = x - xx;
                int dy = y - yy;
                return Math.sqrt(dx * dx + dy * dy);
            }

            public void paint(Graphics g) {

                int px = (int)((countDown * oldX + (maxCount - countDown) * x) /
                        maxCount);
                int py = (int)((countDown * oldY + (maxCount - countDown) * y) /
                        maxCount);
                g.setColor(c);
                g.fillOval(px - rad, py - rad, 2 * rad + 1, 2 * rad + 1);

                //g.setColor(Color.white);
                //g.drawOval(px - rad, py - rad, 2 * rad + 1, 2 * rad + 1);

            }



        }

        class MouseControl extends MouseAdapter {




            public void mousePressed(MouseEvent e) {
                waitForClick = false;
                int mx = e.getX();//store click location for use with buttons
                int my = e.getY();
                main.mainMouse(mx, my, true, false);
                b.mouse(mx, my);
            }

            public void mouseReleased(MouseEvent e) {
                int mx = e.getX();//store click location for use with buttons
                int my = e.getY();
                main.mainMouse(mx, my, false, true);//this will go
            }


        }



        class MouseMove extends MouseMotionAdapter {

            public void mouseDragged(MouseEvent e) {
                int mx = e.getX();//store click location for use with buttons
                int my = e.getY();
                main.mainMouse(mx, my, false, false);
            }

            public void mouseMoved(MouseEvent e) {
                int mx = e.getX();//store click location for use with buttons
                int my = e.getY();
                main.mainMouse(mx, my, false, false);
            }

        }

        class KeyBoard extends KeyAdapter {
            public void keyPressed(KeyEvent e) {
                String k = e.getKeyText(e.getKeyCode());
                char cc = e.getKeyChar();

                main.mainKey(cc, k);

            }
        }

        class Comp {
            int x;
            int y;
            int height;
            int width;

            boolean removeMe = false;

            boolean actionToReport = false;
            Vector subComps = new Vector();


            Window owner;

            DisplayScheme d;

            public Comp() {
                //blank component
            }

            public Comp(int _x, int _y, int _width, int _height, DisplayScheme _d) {

                mainSet(_x, _y, _width, _height, _d);
            }


            public void mainSet(int _x, int _y, int _width, int _height,
                                DisplayScheme _d) {
                x = _x;
                y = _y;
                height = _height;
                width = _width;
                d = _d.copy();
                //System.out.println("d.copy null? "+(d==null));
                set();
                for (int i = subComps.size() - 1; i >= 0; i--) {//go backwards
                    //I don't remember why though.
                    Comp c = (Comp) subComps.get(i);
                    c.mainSet(d);
                }

            }


            public void mainSet(DisplayScheme _d) {

                mainSet(x, y, width, height, _d);
            }
            /*
             public void mainSet(DisplayScheme _d) {
             d = _d;
             set();
             for (int i =  subComps.size()-1; i >= 0; i--) {//go backwards
             Comp c = (Comp) subComps.get(i);
             c.mainSet(d);
             }

             }
             */
            public void set() {
                //overridden

            }

            public void addComp(Comp c) {
                if (!hasComp(c)) {
                    subComps.add(c);
                }
            }

            public boolean hasComp(Comp c) {
                return subComps.contains(c);
            }

            public void moveToBack(Comp c) {
                if (subComps.remove(c)) {
                    subComps.add(0, c);
                }
            }

            public void moveToFront(Comp c) {
                if (subComps.remove(c)) {
                    subComps.add(c);
                }
            }

            public Comp getFirst() {
                if (subComps.size() == 0) {
                    return null;
                } else {
                    return (Comp) subComps.get(subComps.size() - 1);
                }
            }

            public boolean removeComp(Comp c) {
                return subComps.remove(c);
            }
            public void removeAllComps() {
                subComps = new Vector(4);
            }


            public boolean mainMouse(int mx, int my, boolean pressed,
                                     boolean released) {
                //	System.out.println("mainmouse "+(d==null));
                boolean clicked = false;
                for (int i = subComps.size() - 1; i >= 0; i--) {//go backwards
                    Comp c = (Comp) subComps.get(i);
                    clicked = c.mainMouse(mx, my, pressed, released);
                    if (c.checkAndClearRemove()) {

                        removeComp(c);
                    }

                    if (clicked) {
                        break;
                    }
                }


                return mouse(mx, my, pressed, released) || clicked;

            }



            public void mainPaint(Graphics g) {


                paint(g);

                for (int i = 0; i < subComps.size(); i++) {
                    Comp c = (Comp) subComps.get(i);
                    c.mainPaint(g);
                }

            }



            public boolean mouse(int mx, int my, boolean pressed, boolean released) {
                //to be overridden
                return onComp(mx, my);

            }

            public void paint(Graphics g) {
                //to be overridden

            }


            public void tick() {
                //to be overridden
            }

            public void mainTick() {
                tick();
                for (int i = 0; i < subComps.size(); i++) {
                    Comp c = (Comp) subComps.get(i);
                    c.mainTick();
                }


            }


            public void key(char c, String k) {
                //to be overridden

            }

            public void mainKey(char cc, String k) {
                for (int i = 0; i < subComps.size(); i++) {
                    Comp c = (Comp) subComps.get(i);
                    c.mainKey(cc, k);
                    if (c.checkAndClearRemove()) {
                        removeComp(c);
                    }
                }
                key(cc, k);

            }

            public void fillWith(Graphics g, Color c) {
                g.setColor(d.lineC);
                g.fillRect(x, y, width, height);
                g.setColor(c);
                g.fillRect(x + d.bdr, y + d.bdr, width - 2 * d.bdr, height - 2 * d.bdr);
            }


            public boolean onComp(int mx, int my) {
                return isIn(mx, my, x, y, width, height);
            }

            public boolean isIn(int mx, int my, int x, int y, int w, int h) {
                return mx >= x && mx < x + w && my >= y && my < y + h;
            }

            public boolean checkAndClearAction() {
                boolean tmp = actionToReport;
                actionToReport = false;
                return tmp;

            }

            public boolean checkAndClearRemove() {
                boolean tmp = removeMe;
                removeMe = false;
                return tmp;

            }

            public void error(String s) {
                System.out.println(s);
            }

        }


        class Window extends Comp {

            XButton xBut;
            MinButton minBut;
            MaxButton maxBut;
            RestButton restBut;
            Vector butts = new Vector();

            MenuBar menu;

            String title;

            boolean topBar = true;

            int oldX;
            int oldY;
            int oldHeight;
            int oldWidth;
            int pos = 0;
            //0--restored
            //1--maximized
            //-1--minimized



            Point dragged = null;
            boolean dx = false;
            boolean dy = false;
            boolean dwidth = false;
            boolean dheight = false;

            boolean mouseArrow = false;



            public void mainSet(int _x, int _y, int _width, int _height,
                                DisplayScheme _d) {
                x = _x;
                y = _y;
                height = _height;
                width = _width;
                d = _d.copy();

                set();



                if (topBar) {

                    xBut.mainSet(x + width - d.box, y, d.box, d.box, d);
                    maxBut.mainSet(x + width - 2 * d.box + d.bdr, y, d.box, d.box, d);
                    restBut.mainSet(x + width - 3 * d.box + 2 * d.bdr, y, d.box, d.box, d);
                    minBut.mainSet(x + width - 4 * d.box + 3 * d.bdr, y, d.box, d.box, d);

                }

                if (menu != null) {
                    if (topBar) {
                        menu.mainSet(x, y + d.box - d.bdr, width, d.box, d);
                    } else {
                        menu.mainSet(x, y , width, d.box, d);
                    }
                }


                for (int i = subComps.size() - 1; i >= 0; i--) {//go backwards
                    Comp c = (Comp) subComps.get(i);
                    c.mainSet(d);
                }

            }


            public void mainSet(DisplayScheme _d) {

                setTopBar(topBar);
                mainSet(x, y, width, height, _d); //method same as the super class
                //repeated though to insure Windows mainSet is called
            }


            public void init(Window _owner) {
                owner = _owner;

                if (owner != null) {
                    x = owner.left() +
                            (int)(Math.random() * (owner.winWidth() - width - 40));
                    y = owner.top() +
                            (int)(Math.random() * (owner.winHeight() - height - 100));



                }
                butts.add(xBut = new XButton());
                butts.add(maxBut = new MaxButton());
                butts.add(restBut = new RestButton());
                butts.add(minBut = new MinButton());

                //if(menu!=null){
                // addComp(menu);
                //}

            }

            public void setTopBar(boolean b) {
                topBar = b;


                if (topBar) {

                    xBut.mainSet(x + width - d.box, y, d.box, d.box, d);
                    maxBut.mainSet(x + width - 2 * d.box + d.bdr, y, d.box, d.box, d);
                    restBut.mainSet(x + width - 3 * d.box + 2 * d.bdr, y, d.box, d.box, d);
                    minBut.mainSet(x + width - 4 * d.box + 3 * d.bdr, y, d.box, d.box, d);

                }

                if (menu != null) {
                    if (topBar) {
                        menu.mainSet(x, y + d.box - d.bdr, width, d.box, d);
                    } else {
                        menu.mainSet(x, y + d.box - d.bdr, width, d.box, d);
                    }
                }



            }

            public void purge() {
                removeAllComps();
            }

            public int left() {
                return x + d.bdr;
            }

            public int top() {
                int ret = y + d.bdr;
                if (menu != null) {
                    ret += d.box - d.bdr;
                }

                if (menu == null) {
                    ret += d.box - d.bdr;
                }
                return ret;
            }

            public int winWidth() {
                return width - 2 * d.bdr;
            }

            public int winHeight() {
                int ret = height - 2 * d.bdr;
                if (menu != null) {
                    ret -= d.box - d.bdr;
                }

                if (menu == null) {
                    ret -= d.box - d.bdr;
                }
                return ret;
            }

            public boolean doButtons(int mx, int my, boolean pressed,
                                     boolean released) {



                if (xBut.checkAndClearAction()) {
                    removeMe = true;
                    return true;
                }
                if (restBut.checkAndClearAction()) {
                    if (pos != 0) {
                        pos = 0;
                        x = oldX;
                        y = oldY;
                        height = oldHeight;
                        width = oldWidth;
                        //System.out.println("rest "+oldX+" "+oldY+" "+oldHeight+" "+oldWidth);
                        set();
                    }
                    return true;
                }

                if (maxBut.checkAndClearAction()) {
                    if (pos != 1) {
                        //  System.out.println("max");
                        if (pos == 0) {

                            oldX = x;
                            oldY = y;
                            oldHeight = height;
                            oldWidth = width;
                            //System.out.println("max "+oldX+" "+oldY+" "+oldHeight+" "+oldWidth);
                        }

                        x = owner.x + owner.d.bdr;
                        y = owner.y + owner.d.box * 2 - owner.d.bdr;
                        width = owner.width - 2 * owner.d.bdr;
                        height = owner.height - owner.d.box * 2;
                        pos = 1;
                        set();
                    }
                    return true;
                }

                if (minBut.checkAndClearAction()) {
                    if (pos != -1) {
                        //   System.out.println("min");
                        if (pos == 0) {

                            oldX = x;
                            oldY = y;
                            oldHeight = height;
                            oldWidth = width;
                            // System.out.println("min "+oldX+" "+oldY+" "+oldHeight+" "+oldWidth);
                        }
                        x = Math.max(owner.x + owner.d.bdr, x);
                        y = Math.max(owner.y + owner.d.box * 2 - owner.d.bdr, y);
                        width = 200;
                        height = d.box;
                        pos = -1;
                        set();
                    }
                    return true;
                }
                return false;


            }





            public boolean mainMouse(int mx, int my, boolean pressed,
                                     boolean released) {




                if (pressed && onComp(mx, my) && owner != null) {
                    //Comp c=(Comp)owner.subComps.get(owner.subComps.size()-1);
                    //perhaps fix this?  The menu bar should always be in front
                    owner.moveToFront(this);
                    //owner.moveToFront(c);
                }

                if (topBar && (pressed || released)) {
                    for (int i = 0; i < butts.size(); i++) {
                        Button b = (Button) butts.get(i);
                        b.mainMouse(mx, my, pressed, released);
                    }

                    if (doButtons(mx, my, pressed, released)) {
                        return true;
                    }
                }



                if (menu != null) {

                    menu.mainMouse(mx, my, pressed, released);
                    if (menu.actionToReport) {

                    }
                }


                if (released) {
                    dragged = null;
                }
                if (dragged == null) {


                    if (topBar && pressed && onComp(mx, my) &&
                            !isIn(mx, my, x + d.bdr, y + d.box, width - 2 * d.bdr,
                                    height - d.box - d.bdr)) {

                        dx = false;
                        dy = false;
                        dwidth = false;
                        dheight = false;
                        dragged = new Point(mx - x, my - y);

                        if (pos == 0) {

                            if (onFrameSide(mx - x, my - y, width)) {
                                dy = true;
                                dheight = true;
                            }

                            if (onFrameSide(my - y, mx - x, height)) {
                                dx = true;
                                dwidth = true;
                            }

                            if (onFrameSide(mx - x, y + height - my, width)) {
                                dheight = true;
                            }

                            if (onFrameSide(my - y, x + width - mx, height)) {
                                dwidth = true;
                            }
                        }
                        if (pos != 1) {//move

                            if (isIn(mx, my, x + d.bdr, y + d.bdr,
                                    width - 4 * d.box + 2 * d.bdr, d.fld)) {
                                dx = true;
                                dy = true;
                            }

                        }
                        return true;
                    }


                } else {

                    if (owner == null || isIn(mx, my, owner.x + owner.d.bdr,
                            owner.x + 2 * owner.d.box - owner.d.bdr,
                            owner.width - 2 * owner.d.bdr,
                            owner.height - 2 * owner.d.box)) {
                        //assume owner has a menu bar
                        int dx2 = 0;
                        int dy2 = 0;
                        int dwidth2 = 0;
                        int dheight2 = 0;

                        if (dx) {
                            dx2 = mx - x - dragged.x;
                        }
                        if (dy) {
                            dy2 = my - y - dragged.y;
                        }

                        if (dwidth) {

                            if (dx) {
                                dwidth2 = -(mx - x - dragged.x);
                            } else {
                                dwidth2 = (mx - x - dragged.x);
                                dragged.x += dwidth2;
                            }
                        }

                        if (dheight) {

                            if (dy) {
                                dheight2 = -(my - y - dragged.y);
                            } else {
                                dheight2 = (my - y - dragged.y);
                                dragged.y += dheight2;
                            }
                        }

                        //setRecursive(dx2,dy2,dwidth2,dheight2);
                        mainSet(x + dx2, y + dy2, width + dwidth2, height + dheight2, d);
                    }
                    return true;
                }




                boolean clicked = false;
                for (int i = subComps.size() - 1; i >= 0; i--) {
                    Comp c = (Comp) subComps.get(i);
                    clicked = c.mainMouse(mx, my, pressed, released);
                    if (c.checkAndClearRemove()) {

                        removeComp(c);
                    }

                    if (clicked) {
                        break;
                    }
                }


                clicked = mouse(mx, my, pressed, released) || clicked;
                return clicked || onComp(mx, my);


			/*
			 for (int i = 0; i < subComps.size(); i++) {
			 Comp c = (Comp) subComps.get(i);
			 c.mainMouse(mx, my, pressed, released);
			 if (c.checkAndClearRemove()) {

			 removeComp(c);
			 }


			 }


			 mouse(mx, my, pressed, released);
			 */
            }








            public boolean onFrameSide(int mx, int my, int width) {
                return isIn(mx, my, 0, 0, width, d.bdr) ||
                        isIn(mx, my, 0, 0, d.bdr, d.box) ||
                        isIn(mx, my, width - d.bdr, 0, d.bdr, d.box);
            }




            public void mainPaint(Graphics g) {



                fillWith(g, d.butC);




                g.setColor(d.lineC);
                g.fillRect(x, y, width, d.box);




                //System.out.println(title+" steady");


                if (topBar) {

                    //System.out.println(title+" ready");


                    g.setColor(d.selC);
                    g.fillRect(x + d.bdr, y + d.bdr, width - 2 * d.bdr, d.fld);
                    g.setColor(d.bgC);
                    g.drawString(title, x + d.bdr + d.pad, y + d.bdr + d.spc);


                    for (int i = 0; i < butts.size(); i++) {
                        //System.out.println(title+" button "+i);
                        Button b = (Button) butts.get(i);
                        b.mainPaint(g);

                    }

                }



                if (pos != -1) {




                    paint(g);

                    Shape sh = g.getClip();

                    g.clipRect(left(), top(), winWidth(), winHeight());




                    //System.out.println(title+" ready2: "+subComps.size());



                    for (int i = 0; i < subComps.size(); i++) {
                        //System.out.println(title+" sub2 "+i);
                        Comp c = (Comp) subComps.get(i);
                        c.mainPaint(g);
                    }




                    g.setClip(sh);

                    if (menu != null) {
                        menu.mainPaint(g);
                    }


                }






            }



            class MaxButton extends Button {

                // public MaxButton(int _x, int _y, int size, DisplayScheme _d) {
                public MaxButton() {
                    super();
                    // x = _x;
                    //// y = _y;
                    //// height = size;
                    // width = size;
                    // d = _d;

                }
                public void morePaint(Graphics g) {

                    g.setColor(d.lineC);

                    if (buttonDown) {
                        g.fillRect(x + 3 * d.bdr, y + 3 * d.bdr, width - 4 * d.bdr,
                                height - 4 * d.bdr);
                        g.setColor(d.butC);
                        g.fillRect(x + 4 * d.bdr, y + 4 * d.bdr, width - 6 * d.bdr,
                                height - 6 * d.bdr);
                    } else {
                        g.fillRect(x + 2 * d.bdr, y + 2 * d.bdr, width - 4 * d.bdr,
                                height - 4 * d.bdr);
                        g.setColor(d.butC);
                        g.fillRect(x + 3 * d.bdr, y + 3 * d.bdr, width - 6 * d.bdr,
                                height - 6 * d.bdr);
                    }
                }
            }

            class MinButton extends Button {

                // public MinButton(int _x, int _y, int size, DisplayScheme _d) {
                public MinButton() {
                    super();
                    //x = _x;
                    // y = _y;
                    // height = size;
                    //width = size;
                    //d = _d;

                }
                public void morePaint(Graphics g) {

                    g.setColor(d.lineC);

                    if (buttonDown) {
                        g.fillRect(x + 2 * d.bdr + d.pad, y + height - 2 * d.bdr,
                                width - 2 * d.bdr - 2 * d.pad, d.bdr);

                    } else {
                        g.fillRect(x + d.bdr + d.pad, y + height - 3 * d.bdr,
                                width - 2 * d.bdr - 2 * d.pad, d.bdr);
                    }
                }
            }

            class RestButton extends Button {

                //public RestButton(int _x, int _y, int size, DisplayScheme _d) {
                public RestButton() {
                    super();
                    // x = _x;
                    // y = _y;
                    // height = size;
                    // width = size;
                    // d = _d;

                }
                public void morePaint(Graphics g) {

                    g.setColor(d.lineC);

                    if (buttonDown) {
                        g.fillRect(x + 3 * d.bdr + d.pad + d.bdr, y + 2 * d.bdr + d.bdr,
                                width - 5 * d.bdr - d.pad, width - 5 * d.bdr - d.pad);
                        g.setColor(d.butC);
                        g.fillRect(x + 4 * d.bdr + d.pad + d.bdr, y + 3 * d.bdr + d.bdr,
                                width - 7 * d.bdr - d.pad, width - 7 * d.bdr - d.pad);
                        g.setColor(d.lineC);
                        g.fillRect(x + 2 * d.bdr + d.bdr, y + 3 * d.bdr + d.pad + d.bdr,
                                width - 5 * d.bdr - d.pad, width - 5 * d.bdr - d.pad);
                        g.setColor(d.butC);
                        g.fillRect(x + 3 * d.bdr + d.bdr, y + 4 * d.bdr + d.pad + d.bdr,
                                width - 7 * d.bdr - d.pad, width - 7 * d.bdr - d.pad);

                    } else {
                        g.fillRect(x + 3 * d.bdr + d.pad, y + 2 * d.bdr,
                                width - 5 * d.bdr - d.pad, width - 5 * d.bdr - d.pad);
                        g.setColor(d.butC);
                        g.fillRect(x + 4 * d.bdr + d.pad, y + 3 * d.bdr,
                                width - 7 * d.bdr - d.pad, width - 7 * d.bdr - d.pad);
                        g.setColor(d.lineC);
                        g.fillRect(x + 2 * d.bdr, y + 3 * d.bdr + d.pad,
                                width - 5 * d.bdr - d.pad, width - 5 * d.bdr - d.pad);
                        g.setColor(d.butC);
                        g.fillRect(x + 3 * d.bdr, y + 4 * d.bdr + d.pad,
                                width - 7 * d.bdr - d.pad, width - 7 * d.bdr - d.pad);

                    }
                }
            }

            class XButton extends Button {
                Polygon theX;
                // public XButton(int _x, int _y, int size, DisplayScheme _d) {
                public XButton() {
                    super();
                    //  set(_x, _y, size,size,d);
                }

                public void set() {
                    makePoly();
                }

                public void makePoly() {
                    int size = width - 2 * d.bdr;
                    theX = new Polygon();
                    theX.addPoint((int)(.2 * size), (int)(.1 * size));
                    theX.addPoint((int)(.1 * size), (int)(.2 * size));
                    theX.addPoint((int)(.4 * size), (int)(.5 * size));

                    theX.addPoint((int)(.1 * size), (int)(.8 * size));
                    theX.addPoint((int)(.2 * size), (int)(.9 * size));
                    theX.addPoint((int)(.5 * size), (int)(.6 * size));

                    theX.addPoint((int)(.8 * size), (int)(.9 * size));
                    theX.addPoint((int)(.9 * size), (int)(.8 * size));
                    theX.addPoint((int)(.6 * size), (int)(.5 * size));

                    theX.addPoint((int)(.9 * size), (int)(.2 * size));
                    theX.addPoint((int)(.8 * size), (int)(.1 * size));
                    theX.addPoint((int)(.5 * size), (int)(.4 * size));

                    theX.translate(x + d.bdr , y + d.bdr);

                }

                // public void setRecursive(int dx, int dy, int width, int height) {
                //   x += dx;
                //    y += dy;
                //    theX.translate(dx , dy);
                // }
                public void morePaint(Graphics g) {

                    g.setColor(d.lineC);

                    if (buttonDown) {
                        theX.translate(d.bdr, d.bdr);
                        g.fillPolygon(theX);
                        theX.translate(-d.bdr, -d.bdr);
                    } else {

                        g.fillPolygon(theX);
                    }
                }
            }
        }

        class DropDownWindow extends Window {
            DropDownMenu main;
            TextButton okBut;
            // String msg;
            public DropDownWindow(String _title, Window _owner, Vector _choices) {

                super();
                d = _owner.d;
                height = 2 * d.box + d.pad + d.bdr + d.fld;
                width = 300;

                title = _title;
                // msg = _msg;
                addComp(okBut = new TextButton("OK"));
                addComp(main = new DropDownMenu(_choices));
                init(_owner);

            }

            public void set() {
                if (pos == 0) {
                    int minHeight = 3 * d.pad + 3 * d.bdr + 3 * d.box;
                    int minWidth = 400;
                    if (height < minHeight) {
                        height = minHeight;
                    }
                    if (width < minWidth) {
                        width = minWidth;
                    }
                }
                okBut.mainSet(x + width / 2 - 40,
                        y + height - d.box - d.pad - d.bdr, 80, 0, d);
                main.mainSet(x + width / 2 - 140, y + d.box + d.pad, 280, 0, d);
            }


            public int getVal() {
                return main.selected;
            }

            public void key(char c, String k) {
                if (owner != null) {
                    if (owner.getFirst() != this) {
                        return;
                    }
                }

                if (k == "Enter") {
                    removeMe = true;

                }
            }


            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (okBut.checkAndClearAction()) {
                    removeMe = true;
                }

                if (main.checkAndClearAction()) {
                    actionToReport = true;
                }

                return onComp(mx, my);

            }

            // public void paint(Graphics g) {
            //   g.setColor(d.lineC);
            //   g.drawString(msg, x + 10, y + d.box + d.pad + d.txt);
            // }
        }


        class ScrollWindow extends Window {

            ScrollBar main;
            TextButton okBut;


            public ScrollWindow(String _title, Window _owner, int _minVal,
                                int _maxVal, int _incVal, int _repVal) {

                super();
                d = _owner.d;


                title = _title;
                // msg = _msg;
                addComp(okBut = new TextButton("OK"));
                addComp(main = new ScrollBar(false, _minVal, _maxVal, _incVal, _repVal));

                height = 3 * d.pad + 3 * d.bdr + d.box + main.height + okBut.height;
                width = 300;

                init(_owner);

            }

            public void set() {



                if (pos == 0) {
                    int minHeight = 3 * d.pad + 3 * d.bdr + 3 * d.box;
                    int minWidth = 300;


                    if (height < minHeight) {

                        height = minHeight;
                    }

                    if (width < minWidth) {
                        width = minWidth;
                    }
                }
                main.mainSet(x + d.pad + d.bdr, y + d.box + d.pad,
                        width - 2 * d.bdr - 2 * d.bdr, 0, d);

                okBut.mainSet(x + width / 2 - 40,
                        y + height - d.box - d.pad - d.bdr, 80, 0, d);



            }



            public void key(char c, String k) {
                if (owner != null) {
                    if (owner.getFirst() != this) {
                        return;
                    }
                }

                if (k == "Enter") {
                    removeMe = true;

                }
            }


            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (okBut.checkAndClearAction()) {
                    removeMe = true;
                }

                if (main.checkAndClearAction()) {
                    actionToReport = true;
                }

                return onComp(mx, my);

            }


        }

        class AnnounceBox extends Window {


            TextButton okBut;

            String msg;


            public AnnounceBox(String _title, String _msg, Window _owner) {



                super();
                d = _owner.d;
                height = 2 * d.box + d.pad + d.bdr + d.fld;
                width = 300;



                title = _title;
                msg = _msg;
                addComp(okBut = new TextButton("OK"));
                init(_owner);

            }

            public void set() {

                if (pos == 0) {




                    int minHeight = 2 * d.box + d.pad + d.bdr + d.fld;
                    int minWidth = 200;

                    //fixthis?
                    //resizing from the upper left to smaller than minimum size
                    //wil cause the component to move.
                    if (height < minHeight) {
                        //y=y+height-minHeight;
                        height = minHeight;
                    }

                    if (width < minWidth) {
                        // x=x+width-minWidth;
                        width = minWidth;
                    }


                }

                okBut.mainSet(x + width / 2 - 40,
                        y + height - d.box - d.pad - d.bdr, 80, 0, d);



            }




            public void key(char c, String k) {
                if (owner != null) {
                    if (owner.getFirst() != this) {
                        return;
                    }
                }
                if (k == "Enter") {
                    removeMe = true;
                }
            }


            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (okBut.checkAndClearAction()) {
                    removeMe = true;
                }

                return onComp(mx, my);

            }

            public void paint(Graphics g) {
                g.setColor(d.lineC);
                g.drawString(msg, x + 10, y + d.box + d.pad + d.txt);
            }
        }

        class DisplayScheme {
            Color lineC;//outlines
            Color bgC;//backgrounds
            Color butC;//buttons
            Color selC;//selected items

            Font f;

            int txt;//font size
            int bdr;//border size
            int pad;//space between text and
            int fld;//size of text field
            int box;//size of fld with borders
            int spc;//line spacing


            public DisplayScheme(Color _lineC, Color _bgC, Color _butC,
                                 Color _selC, int _txt, int _bdr, int _pad) {
                lineC = _lineC;
                bgC = _bgC;
                butC = _butC;
                selC = _selC;
                txt = _txt;
                bdr = _bdr;
                pad = _pad;
                init();
            }

            public void init() {
                fld = txt + 2 * pad;
                box = fld + 2 * bdr;
                spc = txt + pad;
                f = new Font("COURIER",1, txt);
            }


            public DisplayScheme copy() {
                return new DisplayScheme(lineC, bgC, butC, selC, txt, bdr, pad);
            }

        }


        class Menu extends Comp {
            Vector choice = new Vector();//strings deciding choice
            int selected;
            boolean[] disabled;

            public Menu(Vector _choice) {
                super();

                choice = _choice;
                disabled = new boolean[choice.size()];
                selected = 0;

            }

            public void set() {
                height = 2 * d.bdr + choice.size() * (d.pad + d.txt);
            }

            public void paint(Graphics g) {

                Shape sh = g.getClip();

                g.setClip(x, y, width , height);


                g.setFont(d.f);
                g.setColor(d.lineC);
                g.fillRect(x, y, width, d.bdr + choice.size() * (d.pad + d.txt));
                for (int i = 0; i < choice.size(); i++) {
                    Color textCol;
                    if (i == selected) {
                        if (disabled[i]) {
                            textCol = d.butC;
                        } else {
                            textCol = d.bgC;
                        }

                        g.setColor(d.selC);
                    } else {
                        if (disabled[i]) {
                            textCol = d.butC;
                        } else {
                            textCol = d.lineC;
                        }
                        g.setColor(d.bgC);
                    }
                    g.fillRect(x + d.bdr, y + i * (d.pad + d.txt), width - 2 * d.bdr,
                            d.pad + d.txt);
                    g.setColor(textCol);
                    g.drawString((String) choice.get(i), x + d.bdr + d.pad,
                            y + (i + 1) * (d.pad + d.txt) - d.pad / 2);
                }


                g.setClip(sh);

            }

            public boolean setDisable(String s, boolean b) {
                //return true if successful
                int i = choice.indexOf(s);
                if (i != -1) {
                    disabled[i] = b;
                    return true;
                }
                return false;
            }



            public boolean mouse(int mx, int my, boolean pressed, boolean released) {
                if (onComp(mx, my)) {

                    for (int i = 0; i < choice.size(); i++) {
                        if (isIn(mx, my, x + d.bdr, y + i * (d.pad + d.txt),
                                width - 2 * d.bdr, d.pad + d.txt)) {
                            selected = i;
                            if (!disabled[i]) {
                                if (released) {
                                    actionToReport = true;
                                    removeMe = true;
                                }

                            }
                        }
                    }


                    return true;
                } else {
                    selected = -1;
                    if (pressed) {
                        removeMe = true;
                    }
                    return false;
                }
            }
        }

        class MenuBar extends Comp {
            Vector theMenus = new Vector();
            Vector theNames = new Vector();
            int currMenu;

            int nextMenu;


            public MenuBar() {
                super();

            }

            public void set() {

                for (int i = 0; i < theMenus.size(); i++) {
                    Menu m = (Menu) theMenus.get(i);
                    m.mainSet(getPos(i), y + d.box - d.bdr, 150, 0, d);
                }

                height = d.box;
            }

            public void addMenu(String name, Vector choices) {
                theNames.add(name);
                Menu m = new Menu(choices);
                theMenus.add(m);
            }

            public int getWidth(int n) {
                return 2 * d.pad + (int)((7.0 / 12.0) * d.txt *
                        ((String) theNames.get(n)).length());
            }

            public int getPos(int n) {
                int res = x + d.bdr;
                while (n > 0) {
                    res += getWidth(n - 1);
                    n--;
                }
                return res;
            }

            public boolean mouse(int mx, int my, boolean pressed, boolean released) {



                for (int i = 0; i < theMenus.size(); i++) {
                    Menu m = (Menu) theMenus.get(i);
                    if (m.checkAndClearAction()) {
                        actionToReport = true;

                        return true;
                    }
                }
                if (!onComp(mx, my)) {
                    return false;
                }
                if (!subComps.isEmpty() || pressed) {

                    for (int i = 0; i < theMenus.size(); i++) {

                        if (isIn(mx, my, getPos(i), y, getWidth(i), height)) {
                            removeAllComps();
                            Menu m = (Menu) theMenus.get(i);
                            addComp(m);
                            m.selected = -1;
                            currMenu = i;

                        }
                    }
                }

                return true;
            }



            public boolean setDisable(String s, boolean b) {
                //return true if successful

                for (int i = 0; i < theMenus.size(); i++) {
                    Menu m = (Menu) theMenus.get(i);
                    if (m.setDisable(s, b)) {
                        return true;
                    }
                }
                return false;
            }



            public int getOption() {
                Menu m = (Menu) theMenus.get(currMenu);
                return m.selected;
            }


            public void paint(Graphics g) {
                g.setFont(d.f);
                fillWith(g, d.butC);

                for (int i = 0; i < theNames.size(); i++) {
                    String s = (String) theNames.get(i);
                    if (!subComps.isEmpty() && i == currMenu) {
                        g.setColor(d.selC);
                        g.fillRect(getPos(i), y + d.bdr, getWidth(i), d.fld);
                        g.setColor(d.bgC);
                        g.drawString(s, getPos(i) + d.pad + d.bdr,
                                y + 2 * d.bdr + d.pad + d.txt);
                    } else {
                        g.setColor(d.lineC);
                        g.drawString(s, getPos(i) + d.pad, y + d.bdr + d.pad + d.txt);
                    }


                }
                //   subPaint(g);
            }


        }

        class DropDownMenu extends Comp {
            TriButton b;
            Menu m;
            int selected;

            public DropDownMenu(Vector choice) {
                super();
                selected = 0;
                b = new TriButton('s');
                m = new Menu(choice);
                addComp(b);
            }

            public void set() {
                height = d.box;
                b.mainSet(x + width - d.box, y, d.box, d.box, d);
                m.mainSet(x, y + d.box, width, 0, d);
            }

            public void setVal(int i) {
                selected = i;
                m.selected = i;
            }


            public void paint(Graphics g) {
                fillWith(g, d.bgC);
                g.setFont(new Font("COURIER",1, d.txt));
                g.setColor(d.lineC);
                g.drawString(getText(), x + d.bdr + d.pad, y + d.bdr + d.pad + d.txt);
            }

            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (m.checkAndClearAction()) {
                    actionToReport = true;
                    selected = m.selected;
                    return true;
                }

                if (pressed && onComp(mx, my)) {//box
                    addComp(m);
                    //return true;//fixxthis?
                }
                return false;
            }

            public String getText() {

                if (selected < 0) {
                    selected = 0;
                }
                if (selected >= m.choice.size()) {

                    return "";
                }
                return (String) m.choice.get(selected);
            }



        }

        class Button extends Comp {

            boolean buttonDown = false;
            boolean buttonWatching = false;


            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (!isIn(mx, my, x + d.bdr, y + d.bdr, width - 2 * d.bdr,
                        height - 2 * d.bdr)) {
                    buttonDown = false;
                    if (released) {
                        buttonWatching = false;
                    }
                    return false;
                }

                if (pressed) {
                    buttonWatching = true;
                }

                if (buttonWatching) {
                    buttonDown = true;
                    if (released) {
                        actionToReport = true;
                        buttonWatching = false;
                        buttonDown = false;
                    }
                }
                return true;
            }

            public void paint(Graphics g) {



                if (buttonDown) {
                    g.setColor(d.selC);

                } else {
                    g.setColor(d.lineC);
                }
                g.fillRect(x, y, width, height);
                g.setColor(d.bgC);
                g.fillRect(x + d.bdr, y + d.bdr, width - 2 * d.bdr, height - 2 * d.bdr);

                morePaint(g);

            }
            public void morePaint(Graphics g) {
                //to be overridden
            }




        }

        class TextButton extends Button {
            String s;

            public TextButton(String _s) {
                super();




                s = _s;

            }

            public void set() {
                height = d.box;
            }


            public void morePaint(Graphics g) {

                g.setColor(d.lineC);


                int xx = x + width / 2 - (int)(s.length() * 7.0 / 12.0 * d.txt / 2.0);

                if (buttonDown) {

                    g.drawString(s, xx + d.bdr, y + 2 * d.bdr + d.pad + d.txt);
                } else {

                    g.drawString(s, xx, y + d.bdr + d.pad + d.txt);

                }

            }

        }



        class TriButton extends Button {


            Polygon tri;

            char dir;


            public TriButton(char _dir) {

                super();
                dir = _dir;
            }

            public void set() {
                makePoly();
            }

            public void makePoly() {
                int size = width - 2 * d.bdr;
                tri = new Polygon();
                if (dir == 's') {
                    tri.addPoint((int)(.18 * size), (int)(.3 * size));
                    tri.addPoint((int)(.82 * size), (int)(.3 * size));
                    tri.addPoint((int)(.50 * size), (int)(.75 * size));
                } else if (dir == 'n') {
                    tri.addPoint((int)(.18 * size), (int)(.75 * size));
                    tri.addPoint((int)(.82 * size), (int)(.75 * size));
                    tri.addPoint((int)(.50 * size), (int)(.3 * size));
                } else if (dir == 'e') {
                    tri.addPoint((int)(.3 * size), (int)(.18 * size));
                    tri.addPoint((int)(.3 * size), (int)(.82 * size));
                    tri.addPoint((int)(.75 * size), (int)(.50 * size));
                } else {//'w'
                    tri.addPoint((int)(.75 * size), (int)(.18 * size));
                    tri.addPoint((int)(.75 * size), (int)(.82 * size));
                    tri.addPoint((int)(.3 * size), (int)(.50 * size));
                }
                tri.translate(x + d.bdr , y + d.bdr);

            }



            public void morePaint(Graphics g) {

                g.setColor(d.lineC);

                if (buttonDown) {
                    tri.translate(d.bdr, d.bdr);
                    g.fillPolygon(tri);
                    tri.translate(-d.bdr, -d.bdr);
                } else {

                    g.fillPolygon(tri);
                }
            }

        }

        class CheckBox extends Comp {

            Polygon theCheck;
            boolean checked = false;
            boolean buttonDown = false;
            boolean buttonWatching = false;

            public CheckBox(boolean _checked) {
                super();
                checked = _checked;
            }

            public boolean mouse(int mx, int my, boolean pressed, boolean released) {

                if (!isIn(mx, my, x + d.bdr, y + d.bdr, width - 2 * d.bdr,
                        height - 2 * d.bdr)) {
                    buttonDown = false;
                    if (released) {
                        buttonWatching = false;
                    }
                    return false;
                }

                if (pressed) {
                    buttonWatching = true;
                }

                if (buttonWatching) {
                    buttonDown = true;
                    if (released) {
                        actionToReport = true;
                        buttonWatching = false;
                        buttonDown = false;
                        checked = !checked;
                    }
                }
                return true;
            }

            public void paint(Graphics g) {

                if (buttonDown) {
                    g.setColor(d.selC);

                } else {
                    g.setColor(d.lineC);
                }
                g.fillRect(x, y, width, height);
                g.setColor(d.bgC);
                g.fillRect(x + d.bdr, y + d.bdr, width - 2 * d.bdr, height - 2 * d.bdr);
                if (checked) {
                    g.setColor(d.lineC);
                    g.fillPolygon(theCheck);
                }
            }

            public void set() {
                height = d.box;
                width = d.box;
                makePoly();
            }

            public void makePoly() {
                int size = width - 2 * d.bdr;
                theCheck = new Polygon();

                theCheck.addPoint((int)(.75 * size), (int)(.05 * size));
                theCheck.addPoint((int)(.3 * size), (int)(.6 * size));
                theCheck.addPoint((int)(.15 * size), (int)(.45 * size));
                theCheck.addPoint((int)(.05 * size), (int)(.6 * size));
                theCheck.addPoint((int)(.3 * size), (int)(.95 * size));
                theCheck.addPoint((int)(.95 * size), (int)(.25 * size));

                theCheck.translate(x + d.bdr , y + d.bdr);

            }

        }

        class ScrollBar extends Comp {


            boolean isVert;

            double minVal;
            double maxVal;
            double currVal;
            double incVal;
            double repVal;//controls thickness of bead.  0 means the bead is square.

            int px, py;
            int bx, by;

            boolean active;//being dragged


            TriButton plus;
            TriButton minus;


            public ScrollBar(boolean _isVert, int _minVal, int _maxVal,
                             int _incVal, int _repVal) {

                super();

                minVal = _minVal;
                maxVal = _maxVal;
                currVal = _minVal;
                incVal = _incVal;
                repVal = _repVal;
                isVert = _isVert;


                if (_isVert) {
                    minus = new TriButton('n');
                    plus = new TriButton('s');
                } else {
                    minus = new TriButton('w');
                    plus = new TriButton('e');
                }



                addComp(plus);
                addComp(minus);


            }


            public void set() {

                if (isVert) {

                    width = d.box;
                    minus.mainSet(x, y, width, width, d);
                    plus.mainSet(x, y + height - d.box, width, width, d);

                } else {

                    height = d.box;
                    minus.mainSet(x, y, height, height, d);
                    plus.mainSet(x + width - d.box, y, height, height, d);
                }

                setBeadSize();
                setBeadLoc();

            }



            public void paint(Graphics g) {



                fillWith(g, d.butC);

                g.setColor(d.lineC);
                g.fillRect(px, py, bx, by);
                g.setColor(d.selC);
                g.fillRect(px + d.bdr, py + d.bdr, bx - 2 * d.bdr, by - 2 * d.bdr);


            }

            public void tick() {
                if (minus.buttonDown) {
                    currVal -= incVal;
                    setBeadLoc();
                }


                if (plus.buttonDown) {
                    currVal += incVal;
                    setBeadLoc();
                }



            }

            public boolean mouse(int mx, int my, boolean pressed, boolean released) {



                if (onComp(mx, my)) {




                    if (plus.checkAndClearAction()) {
                        currVal += incVal;
                        setBeadLoc();
                    }

                    if (minus.checkAndClearAction()) {
                        currVal -= incVal;
                        setBeadLoc();
                    }




                    if (pressed) {
                        if (isVert) {
                            if (isIn(mx, my, x + d.bdr, y + d.box, width - 2 * d.bdr,
                                    height - 2 * d.box)) {

                                active = true;

                            }

                        } else {

                            if (isIn(mx, my, x + d.box, y + d.bdr, width - 2 * d.box,
                                    height - 2 * d.bdr)) {

                                active = true;

                            }



                        }
                    }

                }

                if (active) {
                    if (isVert) {
                        py = my - by / 2;
                    } else {

                        px = mx - bx / 2;
                    }
                    setValue();


                    if (released) {
                        active = false;
                    }
                    return true;
                }
                if (onComp(mx, my)) {
                    return true;
                } else {
                    return false;
                }


            }




            public void setBeadLoc() {



                currVal = Math.max(Math.min(maxVal - repVal, currVal), minVal);
                if (isVert) {
                    px = x + d.bdr;
                    if (repVal >= (maxVal - minVal)) {
                        py = y + d.box;
                    } else {
                        int scrollLen = height - 2 * d.box - by;
                        double percent = (currVal - minVal) / ((maxVal - repVal) - minVal);
                        py = (int)(percent * scrollLen + d.box + y);
                    }
                } else {
                    py = y + d.bdr;
                    if (repVal >= (maxVal - minVal)) {
                        px = x + d.box;
                    } else {
                        int scrollLen = width - 2 * d.box - bx;
                        double percent = (currVal - minVal) / ((maxVal - repVal) - minVal);
                        px = (int)(percent * scrollLen + d.box + x);
                    }
                }
            }

            public void setBeadSize() {

                // if (min) {

                //  currVal = minVal;

                //} else {
                //    currVal = maxVal;
                // }


                if (repVal == 0) {
                    bx = by = d.fld;
                } else {
                    if (isVert) {
                        bx = d.fld;
                        if (repVal >= (maxVal - minVal)) {
                            by = height - 2 * d.box;
                        } else {


                            double percent = repVal / (maxVal - minVal);
                            by = (int)(percent * (height - 2 * d.box));
                        }

                    } else {
                        by = d.fld;
                        if (repVal >= (maxVal - minVal)) {
                            bx = width - 2 * d.box;
                        } else {
                            double percent = repVal / (maxVal - minVal);
                            bx = (int)(percent * (width - 2 * d.box));
                        }


                    }

                }
                setBeadLoc();
            }

            public void setValue() {

                if (isVert) {
                    py = Math.max(Math.min(y + height - d.box - by, py), y + d.box);


                    if (maxVal - minVal <= repVal) {
                        currVal = 0;
                    }

                    double scrollLen = height - 2 * d.box - by;
                    double percent = ((double)(py - y - d.box)) / scrollLen;


                    currVal = percent * (maxVal - repVal - minVal) + minVal;

                } else {
                    px = Math.max(Math.min(x + width - d.box - bx, px), x + d.box);

                    if (maxVal - minVal <= repVal) {
                        currVal = 0;
                    }

                    double scrollLen = width - 2 * d.box - bx;
                    double percent = ((double)(px - x - d.box)) / scrollLen;


                    currVal = percent * (maxVal - repVal - minVal) + minVal;



                }


            }


            public boolean isIn(int mx, int my, int x, int y, int w, int h) {
                return mx >= x && mx < x + w && my >= y && my < y + h;
            }




        }

    }
