#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include<GL/glut.h>

#define BLACK 0, 0, 0

//make a global variable -- for tracking the anglular position of camera
double cameraAngle;	//in radian
double rectAngle;	//in degree

void display(){

	//clear the display
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(BLACK, 0);	//color black
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	/********************
	/ set-up camera here
	********************/
	//load the correct matrix -- MODEL-VIEW matrix
	glMatrixMode(GL_MODELVIEW);

	//initialize the matrix
	glLoadIdentity();

	//now give three info
	//1. where is the camera (viewer)?
	//2. where is the camera is looking?
	//3. Which direction is the camera's UP direction?

	//instead of CONSTANT information, we will define a circular path.
//	gluLookAt(-30,-30,50,	0,0,0,	0,0,1);

	gluLookAt(150*cos(cameraAngle), 150*sin(cameraAngle), 50,		0,0,0,		0,0,1);
	//NOTE: the camera still CONSTANTLY looks at the center
	// cameraAngle is in RADIAN, since you are using inside COS and SIN
	
	
	//again select MODEL-VIEW
	glMatrixMode(GL_MODELVIEW);


	/****************************
	/ Add your objects from here
	****************************/
	//add objects
	//rotate this rectangle around the Z axis

	glPushMatrix();{	//STORE the state
		glRotatef(rectAngle,	0,0,1);	// in DEGREE
		//a simple rectangles
		glColor3f(0.3, 0.4, 0.8);
		glBegin(GL_QUADS);{
			glVertex3f(0,0,30);
			glVertex3f(10,0,30);
			glVertex3f(10,20,30);
			glVertex3f(0,20,30);
		}glEnd();
	}glPopMatrix();		//the effect of rotation is not there now.

	printf("%lf\n", rectAngle);

/*	//FORGET THE FIELD
	//an square field
	glColor3f(0.4, 1, 0.4);
	glBegin(GL_QUADS);{
		glVertex3f(-100,-100,0);
		glVertex3f(100,-100,0);
		glVertex3f(100,100,0);
		glVertex3f(-100,100,0);
	}glEnd();
*/

	//some gridlines along the field
	int i;

	glColor3f(0.3, 0.3, 0.3);	//grey
	glBegin(GL_LINES);{
		for(i=-10;i<=10;i++){

			if(i==0)
				continue;	//SKIP the MAIN axes

			//lines parallel to Y-axis
			glVertex3f(i*10, -100, 0);
			glVertex3f(i*10,  100, 0);

			//lines parallel to X-axis
			glVertex3f(-100, i*10, 0);
			glVertex3f( 100, i*10, 0);
		}
	}glEnd();

	// draw the two AXES
	glColor3f(1, 1, 1);	//100% white
	glBegin(GL_LINES);{
		//Y axis
		glVertex3f(0, -150, 0);	// intentionally extended to -150 to 150, no big deal
		glVertex3f(0,  150, 0);

		//X axis
		glVertex3f(-150, 0, 0);
		glVertex3f( 150, 0, 0);
	}glEnd();


	//ADD this line in the end --- if you use double buffer (i.e. GL_DOUBLE)
	glutSwapBuffers();
}

void animate(){
	//codes for any changes in Camera

//	cameraAngle += 0.002;	// camera will rotate at 0.002 radians per frame.	// keep the camera steady NOW!!
	
	//codes for any changes in Models
	
	rectAngle += 1;

	//MISSING SOMETHING? -- YES: add the following
	glutPostRedisplay();	//this will call the display AGAIN
}

void init(){
	//codes for initialization
	cameraAngle = 0;	//// init the cameraAngle
	rectAngle = 0;

	//clear the screen
	glClearColor(BLACK, 0);

	/************************
	/ set-up projection here
	************************/
	//load the PROJECTION matrix
	glMatrixMode(GL_PROJECTION);
	
	//initialize the matrix
	glLoadIdentity();

	//give PERSPECTIVE parameters
	gluPerspective(70,	1,	0.1,	10000.0);
	//field of view in the Y (vertically)
	//aspect ratio that determines the field of view in the X direction (horizontally)
	//near distance
	//far distance
}

int main(int argc, char **argv){
	glutInit(&argc,argv);
	glutInitWindowSize(500, 500);
	glutInitWindowPosition(0, 0);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);	//Depth, Double buffer, RGB color

	glutCreateWindow("My OpenGL Program");

	init();

	glEnable(GL_DEPTH_TEST);	//enable Depth Testing

	glutDisplayFunc(display);	//display callback function
	glutIdleFunc(animate);		//what you want to do in the idle time (when no drawing is occuring)

	glutMainLoop();		//The main loop of OpenGL

	return 0;
}
