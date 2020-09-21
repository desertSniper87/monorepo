#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include<GL/glut.h>

void display(){

	//clear the display
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(0,0,0,0);	//color black
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

	//add objects

	//a simple rectangles
	glColor3f(1,0,0);
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
