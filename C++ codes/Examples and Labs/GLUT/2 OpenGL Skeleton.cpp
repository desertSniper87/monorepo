#include<stdio.h>
#include<stdlib.h>
#include<math.h>

#include<GL/glut.h>

void display(){
	//codes for Models, Camera
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

	//Bit mask to select a double buffered window. This overrides GLUT_SINGLE if it is also specified.
	//Bit mask to select a window with a depth buffer. 


    //for buffer you can check this.....    https://open.gl/depthstencils

	/*


http://www.glprogramming.com/red/chapter10.html


  */

	glutCreateWindow("Some Title");

	init();

	glEnable(GL_DEPTH_TEST);	//enable Depth Testing

	glutDisplayFunc(display);	//display callback function
	glutIdleFunc(animate);		//what you want to do in the idle time (when no drawing is occuring)

	glutMainLoop();		//The main loop of OpenGL

	return 0;
}
