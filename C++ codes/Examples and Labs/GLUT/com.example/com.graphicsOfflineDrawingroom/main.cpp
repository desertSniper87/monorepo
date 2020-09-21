#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include<GL/glut.h>

#define BLACK 0, 0, 0
#define WHITE 1, 1, 1

double cameraAngle;    		
double cameraAngleDelta;
double cameraHeight;	
double cameraRadius;
double shepreAngle;	

GLUquadricObj *quadratic, *quadraticWire;

void displayInit(){
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(BLACK, 0);	
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);


	glMatrixMode(GL_MODELVIEW);

	glLoadIdentity();

	gluLookAt(cameraRadius*cos(cameraAngle), cameraRadius*sin(cameraAngle), cameraHeight,		0,0,0,		0,0,1);
	
	glMatrixMode(GL_MODELVIEW);
}

void drawFloor(){
	int x;

	glColor3f(0.4, 0.2, 0.4);	
	glBegin(GL_LINES);{
		for(x=-15;x<=15;x++){
			//lines parallel to Y-axis
			glVertex3f(x*10, -150, 0);
			glVertex3f(x*10,  150, 0);

			//lines parallel to X-axis
			glVertex3f(-150, x*10, 0);
			glVertex3f( 150, x*10, 0);
		}
	}glEnd();
}

void drawScreen(){
	for(int x=0;x<5;x++){
		glPushMatrix();{
			glTranslatef(14*x,0,0);
			glPushMatrix();{
				glColor3f(.5,.8,1);
				glRotatef(45, 0,0,1);
				glTranslatef(-5 ,0,35);
				glScalef(10 ,1, 70 );			
				glutSolidCube(1);
			}glPopMatrix();
			glPushMatrix();{
					glColor3f(.8,.5,.5);
					glRotatef(-45, 0,0,1);
					glTranslatef(5 ,0,35);
					glScalef(10 ,1, 70 );		
					glutSolidCube(1);
			}glPopMatrix();
		}glPopMatrix();
	}

	for(int x=7;x<10;x++){
		glPushMatrix();{
			glTranslatef(14*x,0,0);
			glPushMatrix();{
				glColor3f(.5,.8,1);
				glRotatef(45, 0,0,1);
				glTranslatef(-5 ,0,35);
				glScalef(10 ,1, 70 );			
				glutSolidCube(1);
			}glPopMatrix();
			glPushMatrix();{
					glColor3f(.2,.5,.8);
					glRotatef(-45, 0,0,1);
					glTranslatef(5 ,0,35);
					glScalef(10 ,1, 70 );		
					glutSolidCube(1);
			}glPopMatrix();
		}glPopMatrix();
	}
	
}

void drawWall(){
	glPushMatrix();{
			glColor3f(0,1,1);	
			glTranslatef(0,-150 ,75 -15);
			glScalef(2.1 ,.01, .7 );
			glutSolidCube(140);
	}glPopMatrix();

	glPushMatrix();{
			glColor3f(1,1,1);	
			glTranslatef(-150, 0 ,75 -15);
			glScalef(.01 , 2.1, .7);
			glutSolidCube(140);
	}glPopMatrix();
}

void drawFirePlace() {
	glPushMatrix(); {
		glColor3f(BLACK);
		glTranslatef(-105, 0 ,50 -15);
		glScalef( 0.1, 0.5, 0.2 );
		glutSolidCube(200);
	}glPopMatrix();
}

void drawMirror(){
	float radius = 20;
	float height = 1;

	glPushMatrix();{	
		glColor3f(WHITE);
		float cylinderRadius = 18;
		float cylinderheight = 2;
			glPushMatrix ();
			glTranslatef (0, 0, cylinderheight);
			gluDisk (quadratic, 0, cylinderRadius, 18, 1);
			glPopMatrix ();
			
			gluCylinder (quadratic, cylinderRadius, cylinderRadius, cylinderheight, 18, 8);
			
			glPushMatrix ();
			gluDisk (quadratic, 0, cylinderheight, 18, 1);
			glPopMatrix ();
	}glPopMatrix();

	glPushMatrix();{
			glColor3f(BLACK);				
			glTranslatef(-7.5, -7.5, height);
			glRotatef(90, 0,1,0);
			
			for(int degree = 0; degree <= 180 ; degree += 10){
				float radian = 3.1416 * degree / 180;
				float length = (degree % 20)? 15 : 25;

				glPushMatrix();{			
					glTranslatef(0, ( radius * sin (radian) + 7.5) , ( radius * cos(radian) + 7.5) );
					glRotatef( degree , -1 , 0, 0);
					gluCylinder(quadratic, height, height, length, 32, 32);
				}glPopMatrix();

				glPushMatrix();{			
					glTranslatef(0, -( radius * sin (radian) - 7.5) , -( radius * cos(radian) - 7.5) );
					glRotatef( 180 + degree , -1, 0, 0);
					gluCylinder(quadratic, height, height, length, 32, 32);
				}glPopMatrix();
			}			
	}glPopMatrix();
}

void drawSofa(){

	double equation[4];

	equation[0] = 0;	
	equation[1] = 1;	
	equation[2] = 0;
	equation[3] = 0;

	glClipPlane(GL_CLIP_PLANE0,equation);

	glEnable(GL_CLIP_PLANE0);{
		glPushMatrix();{
			glColor3f(.9,.9,.9);

			float cylinderRadius = 15;
			float cylinderHeight = 5;
				glPushMatrix ();
					glTranslatef (0, 0, cylinderHeight);
					gluDisk (quadratic, 0, cylinderRadius, 18, 1);
				glPopMatrix ();
				
				gluCylinder (quadratic, cylinderRadius, cylinderRadius, cylinderHeight, 18, 8);
				
				glPushMatrix ();
				gluDisk (quadratic, 0, cylinderRadius, 18, 1);
				glPopMatrix ();
		}glPopMatrix();		
	}glDisable(GL_CLIP_PLANE0);

	glPushMatrix();{
		glColor3f(.4,.7,.4);
		glScalef(30 ,30, 5 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(.8,.4,.4);
		glTranslatef(-12.5, 2.5, 5);
		glScalef(5 , 25, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(.8,.4,.4);
		glTranslatef(12.5, 2.5, 5);
		glScalef(5 , 25, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(.4,.7,.4);
		glRotatef(15,1,0,0);
		glTranslatef(0,-11,20);
		glScalef(30 ,5, 30 );			
		glutSolidCube(1);
	}glPopMatrix();


	glColor3f(.1,.1,.1);
	glPushMatrix();{
		glTranslatef(10,10,-12.5);
		glScalef(2 ,2, 20 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glTranslatef(-10,10,-12.5);
		glScalef(2 ,2, 20 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glTranslatef(10,-10,-12.5);
		glScalef(2 ,2, 20 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glTranslatef(-10,-10,-12.5);
		glScalef(2 ,2, 20 );			
		glutSolidCube(1);
	}glPopMatrix();
}

void drawTool(){
	glPushMatrix();{
		glColor3f(.4,.7,.4);
		glScalef(30 ,30, 5 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(10,10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(-10,10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(10,-10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(-10,-10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();
}

void drawSquareMirror() {
	glPushMatrix(); {
		glColor3f(WHITE);
		glTranslatef(-105, 0 ,50 -15);
		glScalef( .2, .8, 1.2 );
		glutSolidCube(10);
	}glPopMatrix();
}

void drawLamp() {
	// Round Table
	glPushMatrix();{
		glColor3f(.4,.7,.4);
		float cylinderRadius = 18;
		float cylinderheight = 2;
			glPushMatrix ();
			glTranslatef (0, 0, cylinderheight);
			gluDisk (quadratic, 0, cylinderRadius, 18, 1);
			glPopMatrix ();
			
			gluCylinder (quadratic, cylinderRadius, cylinderRadius, cylinderheight, 18, 8);
			
			glPushMatrix ();
			gluDisk (quadratic, 0, cylinderheight, 18, 1);
			glPopMatrix ();
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(10,10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(-10,10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(10,-10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	glPushMatrix();{
		glColor3f(1, 0, 0);
		glTranslatef(-10,-10,-7.5);
		glScalef(2 ,2, 10 );			
		glutSolidCube(1);
	}glPopMatrix();

	//Lamp Handle
	glPushMatrix();{
		glColor3f(.4, .5, .5);
		glTranslatef(-0,-0,15);
		glScalef(.5 ,.5, 20 );			
		glutSolidCube(1);
	}glPopMatrix();

	//Lamp cone

	glPushMatrix();{
		glColor3f(.8,.2,.4);
		glTranslatef(-0,-0,15);
		glScalef(.5 ,.5, 20 );	

		float cylinderRadius = 18;
		float cylinderheight = .5;

			glPushMatrix ();
			glTranslatef (0, 0, cylinderheight);
			glPopMatrix ();
			
			gluCylinder (quadratic, cylinderRadius, 0, cylinderheight, 18, 8);
			
			glPushMatrix ();
			gluDisk (quadratic, 0, cylinderheight, 18, 1);
			glPopMatrix ();
	}glPopMatrix();
}

void display(){

	displayInit();
	drawFloor();
	/****************************
	/ Add your objects from here
	****************************/
	//add objects
	glPushMatrix();{
		glTranslatef(0, 45 ,0);
		drawWall();
	}glPopMatrix();

	glPushMatrix();{
		glRotatef(90,0,0,1);
		glTranslatef(0, 150 ,0);
		drawScreen();
	}glPopMatrix();

	glPushMatrix(); {
		glRotatef(90, 0, 0, 1);
		glTranslatef(0, 35, 0);
		drawFirePlace();
	}glPopMatrix();

	glPushMatrix();{
		glTranslatef(120,-100,90);
		glRotatef(90, 1,0,0);
		glScalef(.4,.4,.4);
		drawMirror();
	}glPopMatrix();

	glPushMatrix ();{
		glTranslatef(0,-40,50);
		glScalef(1.5,1.5,1.5);
		drawSofa();
	}glPopMatrix ();

	glPushMatrix ();{
		glTranslatef(60,-40,50);
		glScalef(1.5,1.5,1.5);
		drawSofa();
	}glPopMatrix ();

	glPushMatrix ();{
		glTranslatef(30, 50, 30);
		glScalef(1.5,1.5,1.5);
		drawTool();
	}glPopMatrix ();

	glPushMatrix();{
		glTranslatef(-90,-255,30);
		glRotatef(-90,0,0,1);
		glScalef(1.5,1.5,1.5);
		drawSquareMirror();
	}glPopMatrix();

	glPushMatrix ();{
		glTranslatef(-30, 120, 30);
		glScalef(1.5,1.5,1.5);
		drawLamp();
	}glPopMatrix ();

	glutSwapBuffers();
}

void animate(){
	glutPostRedisplay();	//this will call the display AGAIN
}

void keyboardListener(unsigned char key, int x,int y){
	switch(key){

		case '1':	//reverse the rotation of camera
			cameraAngleDelta = -cameraAngleDelta;
			break;

		case '2':	//increase rotation of camera by 10%
			cameraAngleDelta *= 1.1;
			break;

		case '3':	//decrease rotation
			cameraAngleDelta /= 1.1;
			break;

		case 27:	//ESCAPE KEY -- simply exit
			exit(0);
			break;

		default:
			break;
	}
}

void specialKeyListener(int key, int x,int y){
	switch(key){
		case GLUT_KEY_DOWN:		//down arrow key
			cameraRadius += 10;
			break;
		case GLUT_KEY_UP:		// up arrow key
			if(cameraRadius > 10)
				cameraRadius -= 10;
			break;

		case GLUT_KEY_RIGHT:
			cameraAngle += .2;
			break;
		case GLUT_KEY_LEFT:
			cameraAngle -= .2;
			break;

		case GLUT_KEY_PAGE_UP:
			cameraHeight += 10;
			break;
		case GLUT_KEY_PAGE_DOWN:
			cameraHeight -= 10;
			break;

		case GLUT_KEY_INSERT:
			break;

		case GLUT_KEY_HOME:
			break;
		case GLUT_KEY_END:
			break;

		default:
			break;
	}
}


void init(){
	//codes for initialization
	cameraAngle = 3.1416/2;	//// init the cameraAngle
	cameraAngleDelta = 0.002;
	cameraHeight = 150;
	cameraRadius = 150;

	quadratic = gluNewQuadric();

	quadraticWire = gluNewQuadric();
	gluQuadricDrawStyle(quadraticWire, GLU_LINE);
	

	glClearColor(BLACK, 0);

	glMatrixMode(GL_PROJECTION);
	
	glLoadIdentity();

	gluPerspective(70,	1,	0.1,	10000.0);
}

int main(int argc, char **argv){
	glutInit(&argc,argv);
	glutInitWindowSize(800, 800);
	glutInitWindowPosition(0, 0);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);	//Depth, Double buffer, RGB color

	glutCreateWindow("Drawing Room");

	init();

	glEnable(GL_DEPTH_TEST);	

	glutDisplayFunc(display);	
	glutIdleFunc(animate);		

	glutKeyboardFunc(keyboardListener);
	glutSpecialFunc(specialKeyListener);

	glutMainLoop();		//The main loop of OpenGL

	return 0;
}
