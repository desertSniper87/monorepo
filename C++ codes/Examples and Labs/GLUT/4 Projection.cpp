#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include<GL/glut.h>

#define BLACK 0, 0, 0

void display(){

	//clear the display
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(BLACK, 0);	//color black
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	//add objects

	//a simple rectangles
	glBegin(GL_QUADS);{
		glVertex3f(0,0,30);
		glVertex3f(10,0,30);
		glVertex3f(10,20,30);
		glVertex3f(0,20,30);
	}glEnd();

}

void animate(){
	//codes for any changes in Models, Camera
}

void init(){
	//codes for initialization

	//clear the screen
	glClearColor(BLACK, 0);

	//set-up projection here
	glMatrixMode(GL_PROJECTION);	
	glLoadIdentity();
	gluPerspective(70,	1,	0.1,	100000.0);
	//field of view in the Y (vertically)
	//aspect ratio that determines the field of view in the X direction (horizontally)
	//near distance
	//far distance
}

int main(int argc, char **argv){
	glutInit(&argc,argv);
	glutInitWindowSize(500, 500);
	glutInitWindowPosition(100, 100);
	glutInitDisplayMode(GLUT_DEPTH | GLUT_DOUBLE | GLUT_RGB);	//Depth, Double buffer, RGB color

	glutCreateWindow("My OpenGL Program");

	init();

	glEnable(GL_DEPTH_TEST);	//enable Depth Testing

	glutDisplayFunc(display);	//display callback function
	glutIdleFunc(animate);		//what you want to do in the idle time (when no drawing is occuring)

	glutMainLoop();		//The main loop of OpenGL

	return 0;
}
