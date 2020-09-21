#include<stdio.h>
#include<stdlib.h>
#include<math.h>
#include<windows.h>

#include<GL/glut.h>

#define PI 3.1415
#define piby180 (PI/180.0)

/*============================ Camera =============================*/

struct Vector3d
{
	GLfloat x,y,z;
};

Vector3d get3dVector(GLfloat x, GLfloat y, GLfloat z )
{
	Vector3d tmp;
	tmp.x = x;
	tmp.y = y;
	tmp.z = z;
	return tmp;
}

GLfloat Getget3dVectorLength(Vector3d * v)
{
	return (GLfloat)(sqrt(v->x*v->x+v->y*v->y+v->z*v->z));
}

Vector3d Normalize3dVector(Vector3d v)
{
	Vector3d res;
	float l =Getget3dVectorLength(&v);
	if (l == 0.0f) return get3dVector(0.0f,0.0f,0.0f);
	res.x = v.x / l;
	res.y = v.y / l;
	res.z = v.z / l;
	return res;
}

Vector3d operator+(Vector3d v, Vector3d u)
{
	Vector3d res;
	res.x = v.x+u.x;
	res.y = v.y+u.y;
	res.z = v.z+u.z;
	return res;
}
Vector3d operator-(Vector3d v, Vector3d u)
{
	Vector3d res;
	res.x = v.x-u.x;
	res.y = v.y-u.y;
	res.z = v.z-u.z;
	return res;
}


Vector3d operator*(Vector3d v, float r)
{
	Vector3d res;
	res.x = v.x*r;
	res.y = v.y*r;
	res.z = v.z*r;
	return res;
}

Vector3d CrossProduct(Vector3d * u, Vector3d * v)
{
	Vector3d resVector;
	resVector.x = u->y*v->z - u->z*v->y;
	resVector.y = u->z*v->x - u->x*v->z;
	resVector.z = u->x*v->y - u->y*v->x;

	return resVector;
}
float operator* (Vector3d v, Vector3d u)
{
	return v.x*u.x+v.y*u.y+v.z*u.z;
}


class CAM
{
private:
	Vector3d dir;
	Vector3d RightVector;	
	Vector3d UpVector;
	Vector3d Position;
	GLfloat anglex, angley, anglez;	
public:
	CAM()
	{
		Position = get3dVector(1.0,0,0);
		dir = get3dVector(-1.0,0,0);
		RightVector = get3dVector(0.0,1.0,0.0);
		UpVector = get3dVector(0.0,0.0,1.0);
		anglex=angley=anglez=0.0;
	}
	void Move (Vector3d Direction)
	{
		Position =Position + Direction;
	}
	
	void MoveForward( GLfloat Distance )
	{
		Position = Position + (dir*-Distance);
	}
	void StrafeRight ( GLfloat Distance )
	{
		Position = Position + (RightVector*Distance);
	}

	void MoveUpward( GLfloat Distance )
	{
		Position = Position + (UpVector*Distance);
	}


	void RotateX (GLfloat Angle)
	{
		anglex += Angle;
		dir = Normalize3dVector(dir*cos(Angle*piby180)+ UpVector*sin(Angle*piby180));
		UpVector = CrossProduct(&dir, &RightVector)*-1;
	}

	void RotateY (GLfloat Angle)
	{
		angley += Angle;
		dir = Normalize3dVector(dir*cos(Angle*piby180)-RightVector*sin(Angle*piby180));
		RightVector = CrossProduct(&dir, &UpVector);
	}

	void RotateZ (GLfloat Angle)
	{
		anglez += Angle;
		RightVector = Normalize3dVector(RightVector*cos(Angle*piby180)+ UpVector*sin(Angle*piby180));
		UpVector = CrossProduct(&dir, &RightVector)*-1;
	}


	void Render( void )
	{
		Vector3d ViewPoint = Position+dir;
		gluLookAt(Position.x,Position.y,Position.z,ViewPoint.x,ViewPoint.y,ViewPoint.z,UpVector.x,UpVector.y,UpVector.z);
	}

};


CAM Camera;
double cameraHeight;
double cameraAngle;

int drawgrid;
int drawaxes;
double angle;
double rotate;
double rad=20;
int is_blu;

double angg=30;

double Angle;			//in radian
double AngleDelta;
GLUquadric* IDquadric=gluNewQuadric() ;

GLuint tex1,tex2,tex3,tex4,tex5,tex6,tex7,tex8,tex9,tex10,tex11,tex12,tex13;
int num_texture;
struct point
{
	double x,y,z;
};


Vector3d normal_find(double a,double b,double c,double d,double e,double f,double g,double h,double i){
	Vector3d v1=get3dVector(a-d,b-e,c-f);
	Vector3d v2=get3dVector(a-g,b-h,c-i);
	Vector3d nor=CrossProduct(&v1,&v2);
	nor=Normalize3dVector(nor);
	return nor;
}


void normal_wrapper(double a,double b,double c,double d,double e,double f,double g,double h,double i){
	Vector3d v=normal_find(a,b,c,d,e,f,g,h,i);
	glNormal3f(v.x,v.y,v.z);
}

void normal_z(){
	glNormal3f(0,0,1);
}

void normal_x(){
	glNormal3f(1,0,0);
}
void normal_y(){
	glNormal3f(0,-1,0);
}


void drawAxes()
{
	if(drawaxes==1)
	{
		glColor3f(1.0, 1.0, 1.0);
		glBegin(GL_LINES);{
			glVertex3f( 100,0,0);
			glVertex3f(-100,0,0);
			glVertex3f(0,-100,0);
			glVertex3f(0, 100,0);
			glVertex3f(0,0, 100);
			glVertex3f(0,0,-100);
		}glEnd();
	}
}

/*============================ Camera end ===========================*/


int LoadBitmap(char *filename)
{
    int i, j=0;
    FILE *l_file;
    unsigned char *l_texture;

    BITMAPFILEHEADER fileheader;
    BITMAPINFOHEADER infoheader;
    RGBTRIPLE rgb;

    num_texture++;

    if( (l_file = fopen(filename, "rb"))==NULL) return (-1);

    fread(&fileheader, sizeof(fileheader), 1, l_file);

    fseek(l_file, sizeof(fileheader), SEEK_SET);
    fread(&infoheader, sizeof(infoheader), 1, l_file);

    l_texture = (byte *) malloc(infoheader.biWidth * infoheader.biHeight * 4);
    memset(l_texture, 0, infoheader.biWidth * infoheader.biHeight * 4);

 for (i=0; i < infoheader.biWidth*infoheader.biHeight; i++)
    {
            fread(&rgb, sizeof(rgb), 1, l_file);

            l_texture[j+0] = rgb.rgbtRed;
            l_texture[j+1] = rgb.rgbtGreen;
            l_texture[j+2] = rgb.rgbtBlue;
            l_texture[j+3] = 255;
            j += 4;
    }
    fclose(l_file);

    glBindTexture(GL_TEXTURE_2D, num_texture);

    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);

// glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);
    glTexImage2D(GL_TEXTURE_2D, 0, 4, infoheader.biWidth, infoheader.biHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, l_texture);
     gluBuild2DMipmaps(GL_TEXTURE_2D, 4, infoheader.biWidth, infoheader.biHeight, GL_RGBA, GL_UNSIGNED_BYTE, l_texture);

    free(l_texture);

    return (num_texture);

}

void drawGrid()
{
	int i;
	if(drawgrid==1)
	{
		glColor3f(0.6, 0.6, 0.6);	//grey
		glBegin(GL_LINES);{
			for(i=-8;i<=8;i++){

				if(i==0)
					continue;	//SKIP the MAIN axes

				//lines parallel to Y-axis
				glVertex3f(i*10, -90, 0);
				glVertex3f(i*10,  90, 0);

				//lines parallel to X-axis
				glVertex3f(-90, i*10, 0);
				glVertex3f( 90, i*10, 0);
			}
		}glEnd();
	}
}


void drawSquare(float a)
{
	glBegin(GL_QUADS);{
		glVertex3f( a, a,2);
		glVertex3f( a,-a,2);
		glVertex3f(-a,-a,2);
		glVertex3f(-a, a,2);
	}glEnd();
}


void draw_qboid(double pos[][3]){
	normal_wrapper(pos[0][0],pos[0][1],pos[0][2],pos[1][0],pos[1][1],pos[1][2],pos[2][0],pos[2][1],pos[2][2]);
	glBegin(GL_QUADS);
	for (int i = 0; i < 4; ++i)glVertex3f(pos[i][0],pos[i][1],pos[i][2]);
	glEnd();
	normal_wrapper(pos[4][0],pos[4][1],pos[4][2],pos[5][0],pos[5][1],pos[5][2],pos[6][0],pos[6][1],pos[6][2]);
	glBegin(GL_QUADS);
	for (int i = 4; i < 8; ++i)glVertex3f(pos[i][0],pos[i][1],pos[i][2]);
	glEnd();
	for (int i = 0; i <=2; i++){
		normal_wrapper(pos[i][0],pos[i][1],pos[i][2],pos[i+1][0],pos[i+1][1],pos[i+1][2],pos[i+5][0],pos[i+5][1],pos[i+5][2]);
		glBegin(GL_QUADS);
		glVertex3f(pos[i][0],pos[i][1],pos[i][2]);
		glVertex3f(pos[i+1][0],pos[i+1][1],pos[i+1][2]);
		glVertex3f(pos[i+5][0],pos[i+5][1],pos[i+5][2]);
		glVertex3f(pos[i+4][0],pos[i+4][1],pos[i+4][2]);
		glEnd();
	}
	normal_wrapper(pos[0][0],pos[0][1],pos[0][2],pos[3][0],pos[3][1],pos[3][2],pos[7][0],pos[7][1],pos[7][2]);
	glBegin(GL_QUADS);
	glVertex3f(pos[0][0],pos[0][1],pos[0][2]);
	glVertex3f(pos[3][0],pos[3][1],pos[3][2]);
	glVertex3f(pos[7][0],pos[7][1],pos[7][2]);
	glVertex3f(pos[4][0],pos[4][1],pos[4][2]);
	glEnd();
}


//void draw_side(double breadth,double width,double height,int num,GLuint tex){
//	glColor3f(69.0/255,132.0/255,158.0/255);
//	normal_wrapper(0,0,0,height,0,0,height,width,0);
//	glBegin(GL_QUADS);
//		//face1
//		glVertex3f( 0, 0,0);
//		glVertex3f( height,0,0);
//		glVertex3f( height,width,0);
//		glVertex3f(0, width,0);
//	glEnd();
//	
//	normal_wrapper(0, 0,0,height,0,0,height,0,breadth);
//	glBegin(GL_QUADS);
//		//face2
//		glVertex3f( 0, 0,0);
//		glVertex3f( height,0,0);
//		glVertex3f( height,0,breadth);
//		glVertex3f(0, 0,breadth);
//	glEnd();
//	normal_wrapper(0, width,0,height,width,0,height,width,breadth);
//	glBegin(GL_QUADS);
//		//face3
//		glVertex3f( 0, width,0);
//		glVertex3f( height,width,0);
//		glVertex3f( height,width,breadth);
//		glVertex3f(0, width,breadth);
//	glEnd();
//	
//	normal_wrapper(0, 0,breadth,height,0,breadth,height,width,breadth);
//	glBegin(GL_QUADS);
//		//face4
//		glVertex3f( 0, 0,breadth);
//		glVertex3f( height,0,breadth);
//		glVertex3f( height,width,breadth);
//		glVertex3f(0, width,breadth);
//	glEnd();
//	glColor3f(1,1,1);
//	glEnable(GL_TEXTURE_2D);
//	glBindTexture(GL_TEXTURE_2D,tex);
//	normal_wrapper(0, 0,0,0,0,breadth,0,width,breadth);
//	glBegin(GL_QUADS);
//		//face5
//		glTexCoord2f(0,0);glVertex3f( 0, 0,0);
//		glTexCoord2f(1,0);glVertex3f( 0,0,breadth);
//		glTexCoord2f(1,num);glVertex3f( 0,width,breadth);
//		glTexCoord2f(0,num);glVertex3f(0, width,0);
//	glEnd();
//	normal_wrapper(0, 0,0,0,0,breadth,0,width,breadth);
//	glBegin(GL_QUADS);
//		//face6
//		glTexCoord2f(0,0);glVertex3f( height, 0,0);
//		glTexCoord2f(1,0);glVertex3f( height,0,breadth);
//		glTexCoord2f(1,num);glVertex3f( height,width,breadth);
//		glTexCoord2f(0,num);glVertex3f(height, width,0);
//	glEnd();
//	glDisable(GL_TEXTURE_2D);
//	
//}
//
//
//void draw_rope(double width){
//	double ang1=7,ang2=6;
//	int slice1=9,slice2=5;
//	double rad1_x=63,rad1_y=54;
//	double pos1[10][2];
//	glColor3f(1,1,1);
//	glPushMatrix();
//	double prec=0.01;
//	double width2=width-2*prec;
//	glTranslatef(prec,0,0);
//	double high[]={29,22.6,17,13,9,5};
//	for (int i = 0; i < 6; ++i)
//	{
//		glTranslatef(0,6,0);
//		double ppos[8][3]={{0,0,prec},
//			{width2,0,prec},
//			{width2,width2,prec},
//			{0,width2,prec},
//			{0,0,high[i]},
//			{width2,0,high[i]},
//			{width2,width2,high[i]-1},
//			{0,width2,high[i]-1}
//		};
//		draw_qboid(ppos);
//	}
//	glPopMatrix();
//
//	glColor3f(69.0/255,132.0/255,158.0/255);
//	for (int i = 0; i <= slice1; ++i)
//	{
//		pos1[i][0]=rad1_x*(1-cos((i+2)*ang1*PI/180.0))-3;
//		pos1[i][1]=rad1_y*(1-sin((i+2)*ang1*PI/180.0))-1;
//		if(i==0)continue;
//		double ppos[8][3]={{0,pos1[i][0],pos1[i][1]},
//			{width,pos1[i][0],pos1[i][1]},
//			{width,pos1[i-1][0],pos1[i-1][1]},
//			{0,pos1[i-1][0],pos1[i-1][1]},
//			{0,pos1[i][0]-1.5,pos1[i][1]},
//			{width,pos1[i][0]-1.5,pos1[i][1]},
//			{width,pos1[i-1][0]-1.5,pos1[i-1][1]},
//			{0,pos1[i-1][0]-1.5,pos1[i-1][1]}
//		};
//		draw_qboid(ppos);
//	}
//
//	rad1_x=63*2,rad1_y=2*54;
//	for (int i = 0; i <= slice2; ++i)
//	{
//		pos1[i][0]=rad1_x*(1-cos((i+5)*ang2*PI/180.0))-18;
//		pos1[i][1]=rad1_y*(1-sin((i+5)*ang2*PI/180.0))-14;
//		if(i==0)continue;
//		double ppos[8][3]={{0,pos1[i][0],pos1[i][1]},
//			{width,pos1[i][0],pos1[i][1]},
//			{width,pos1[i-1][0],pos1[i-1][1]},
//			{0,pos1[i-1][0],pos1[i-1][1]},
//			{0,pos1[i][0]-1.5,pos1[i][1]},
//			{width,pos1[i][0]-1.5,pos1[i][1]},
//			{width,pos1[i-1][0]-1.5,pos1[i-1][1]},
//			{0,pos1[i-1][0]-1.5,pos1[i-1][1]}
//		};
//		draw_qboid(ppos);
//	}
//
//}
//
//
//void draw_road1(double surfaces[],double size){
//	// road 1
//	glColor3f(.5,0,.5);
//	glPushMatrix();
//		glTranslatef(0,surfaces[1],surfaces[2]);
//		draw_side(1,size,1.5,22,tex7);
//		glTranslatef(.01,0,0);
//		draw_rope(1.48);
//	glPopMatrix();
//	glPushMatrix();
//		glTranslatef(surfaces[0]-1.5,surfaces[1],surfaces[2]);
//		draw_side(1,size,1.5,22,tex7);
//		glTranslatef(.01,0,0);
//		draw_rope(1.48);
//	glPopMatrix();
//	//road 1 surface
//	glColor3f(.3,.7,.5);
//	glPushMatrix();
//		glTranslatef(1.5,surfaces[1],surfaces[2]);
//		glColor3f(1,1,1);
//		glEnable(GL_TEXTURE_2D);
//		glBindTexture(GL_TEXTURE_2D,tex8);
//		normal_wrapper(0,0,0,surfaces[0]-3,0,0,surfaces[0]-3,size,0);
//		glBegin(GL_QUADS);{
//			glTexCoord2f(0,0);glVertex3f(0,0,0);
//			glTexCoord2f(1,0);glVertex3f(surfaces[0]-3,0,0);
//			glTexCoord2f(1,1);glVertex3f(surfaces[0]-3,size,0);
//			glTexCoord2f(0,1);glVertex3f(0,size,0);
//		}glEnd();
//		glDisable(GL_TEXTURE_2D);
//	glPopMatrix();
//}
//
//
//void draw_railing(double width,double breadth){
//	glColor3f(69.0/255,132.0/255,158.0/255);
//	glPushMatrix();
//	double prec=0.01;
//	double width2=width-2*prec;
//	glTranslatef(prec,0,0);
//	double high[]={6,5,3.8,2.9,1.9,0.8};
//	for (int i = 0; i < 6; ++i)
//	{
//		double ppos[8][3]={{0,0,prec},
//			{width2,0,prec},
//			{width2,1,prec},
//			{0,1,prec},
//			{0,0,-high[i]},
//			{width2,0,-high[i]},
//			{width2,1,-high[i]+.5},
//			{0,1,-high[i]+.5}
//		};
//		draw_qboid(ppos);
//
//		if(i==5)continue;
//		double ppos1[8][3]={{0,1,prec},
//			{width2,1,prec},
//			{width2,1,-1},
//			{0,1,-1},
//			{0,3,-high[i+1]-.2},
//			{width2,3,-high[i+1]-.2},
//			{width2,2.5,-high[i+1]-.4},
//			{0,2.5,-high[i+1]-.4}
//		};
//		draw_qboid(ppos1);
//		glTranslatef(0,4.3,0);
//	}
//	glPopMatrix();
//	glPushMatrix();
//	{
//		glTranslatef(prec,0,0);
//		int i=0;
//		double ppos[8][3]={
//			{0,0,-high[i]},
//			{width2,0,-high[i]},
//			{width2,0,-high[i]+1},
//			{0,0,-high[i]+1},
//			{0,breadth,0},
//			{width2,breadth,0},
//			{width2,breadth-1,0},
//			{0,breadth-1,0}
//		};
//		draw_qboid(ppos);
//	}
//	glPopMatrix();
//}
//
//
//void draw_road2(double surfaces[],double size){
//	// road 2
//	glColor3f(.5,0,.5);
//	glPushMatrix();
//		glTranslatef(1.5,0,surfaces[2]);
//		glRotatef(180,0,0,1);
//		glRotatef(angg,1,0,0);
//		draw_side(1,size,1.5,12,tex7);
//		draw_railing(1.48,size);
//	glPopMatrix();
//	glPushMatrix();
//		//glTranslatef(surfaces[0]-1.5,surfaces[1],surfaces[2]);
//		glTranslatef(surfaces[0],0,surfaces[2]);
//		glRotatef(180,0,0,1);
//		glRotatef(angg,1,0,0);
//		draw_side(1,size,1.5,12,tex7);
//		draw_railing(1.48,size);
//	glPopMatrix();
//	//road 2 surface
//	glColor3f(.3,.7,.5);
//	glPushMatrix();
//		glTranslatef(1.5,0,surfaces[2]);
//		glRotatef(180,0,0,1);
//		glRotatef(angg,1,0,0);
//		glColor3f(1,1,1);
//		glEnable(GL_TEXTURE_2D);
//		glBindTexture(GL_TEXTURE_2D,tex13);
//		normal_wrapper(0,0,0,-surfaces[0]+3,0,0,-surfaces[0]+3,size,0);
//		glBegin(GL_QUADS);{
//			glTexCoord2f(0,0);glVertex3f(0,0,0);
//			glTexCoord2f(1,0);glVertex3f(-surfaces[0]+3,0,0);
//			glTexCoord2f(1,1);glVertex3f(-surfaces[0]+3,size,0);
//			glTexCoord2f(0,1);glVertex3f(0,size,0);
//		}glEnd();
//		glDisable(GL_TEXTURE_2D);
//	glPopMatrix();
//}
//
//void draw_lower_sides(){
//	double surfaces[]={20,22,17};
//	glColor3f(1,1,1);
//	glEnable(GL_TEXTURE_2D);
//	glBindTexture(GL_TEXTURE_2D,tex3);
//	for (int i = 0; i < 2; ++i)
//	{
//		glPushMatrix();
//			if(i)glTranslatef(0,surfaces[1],0);
//			normal_wrapper(0,0,0,0,0,-surfaces[2],-surfaces[0],0,-surfaces[2]);
//			glBegin(GL_QUADS);{
//				glTexCoord2f(0,0);glVertex3f(0,0,0);
//				glTexCoord2f(0,1);glVertex3f(0,0,surfaces[2]);
//				glTexCoord2f(6,1);glVertex3f(surfaces[0],0,surfaces[2]);
//				glTexCoord2f(6,0);glVertex3f(surfaces[0],0,0);
//			}glEnd();
//		glPopMatrix();
//	}
//	glDisable(GL_TEXTURE_2D);
//	//top
//	normal_wrapper(0,0,surfaces[2],surfaces[0],0,surfaces[2],surfaces[0],surfaces[1],surfaces[2]);
//	glBegin(GL_QUADS);{
//		glVertex3f(0,0,surfaces[2]);
//		glVertex3f(surfaces[0],0,surfaces[2]);
//		glVertex3f(surfaces[0],surfaces[1],surfaces[2]);
//		glVertex3f(0,surfaces[1],surfaces[2]);
//	}glEnd();
//	//
//	glPushMatrix();
//	draw_road1(surfaces,80.0);
//	glPopMatrix();
//	glPushMatrix();
//	draw_road2(surfaces,25.0);
//	glPopMatrix();
//}
//
//void draw_lower_half_cylinder(double radius,double slices,double height,GLuint tex){
//	struct point points[2][100];
//	int i,h,j,r;
//	h=0;
//	r=radius;
//	for(i=0;i<=1;i++)
//	{
//		if(i)h=height;
//		for(j=0;j<=slices;j++)
//		{
//			points[i][j].x=r*cos(((double)j/(double)(2*slices))*2*PI);
//			points[i][j].y=r*sin(((double)j/(double)(2*slices))*2*PI);
//			points[i][j].z=h;
//		}
//	}
//
//	i=0;
//	glColor3f(1,1,1);
//	glEnable(GL_TEXTURE_2D);
//	glBindTexture(GL_TEXTURE_2D,tex);
//	for(j=0;j<slices;j++)
//	{
//		//glColor3f((double)colors/(double)(slices),(double)colors/(double)(slices),0);
//		normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z,points[i][j+1].x,points[i][j+1].y,points[i][j+1].z,points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//		glBegin(GL_QUADS);{
//			glTexCoord2f(0,0);glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z);
//			glTexCoord2f(1,0);glVertex3f(points[i][j+1].x,points[i][j+1].y,points[i][j+1].z);
//			glTexCoord2f(1,1);glVertex3f(points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//			glTexCoord2f(0,1);glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z);
//		}glEnd();
//	}
//	glDisable(GL_TEXTURE_2D);
//
//}
//
//
//void draw_lower_half_cylinder_with_boat(double radius,double slices,double height,GLuint tex){
//	struct point points[100][100];
//	int i,h,j,r;
//	h=0;
//	r=radius;
//	int stack=10;
//	double coef[]={.14876,0.146694,0.142562,0.136364,0.128099,0.117769};
//	double add[]={18,17.75,17.25,16.5,15.5,14.25};
//	for(i=0;i<=stack;i++)
//	{
//		h=i*height/stack;
//		for(j=0;j<=slices;j++)
//		{
//			points[i][j].x=r*cos(((double)j/(double)(2*slices))*2*PI);
//			if(i<3)points[i][j].y=-coef[0]*pow(points[i][j].x,2)+add[0];
//			else if(i<8)points[i][j].y=-coef[i-2]*pow(points[i][j].x,2)+add[i-2];
//			else points[i][j].y=r*sin(((double)j/(double)(2*slices))*2*PI);
//			points[i][j].z=h;
//		}
//	}
//
//	glColor3f(1,1,1);
//	glEnable(GL_TEXTURE_2D);
//	glBindTexture(GL_TEXTURE_2D,tex);
//	for (i = 0; i < stack; ++i)
//	{
//		double start=double(i)/stack;
//		double end=double(i+1)/stack;
//		for(j=0;j<slices;j++)
//		{
//			normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z,points[i][j+1].x,points[i][j+1].y,points[i][j+1].z,points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//			glBegin(GL_QUADS);{
//				glTexCoord2f(0,start);glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z);
//				glTexCoord2f(1,start);glVertex3f(points[i][j+1].x,points[i][j+1].y,points[i][j+1].z);
//				glTexCoord2f(1,end);glVertex3f(points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//				glTexCoord2f(0,end);glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z);
//			}glEnd();
//		}
//	}
//	
//	glDisable(GL_TEXTURE_2D);
//
//}
//
//
//void draw_lower_half_cover(double radius1,double radius2,double slices,double height,double colors){
//	struct point points[2][100];
//	int i,h,j,r;
//	r=radius1;
//	h=height;
//	for(i=0;i<=1;i++)
//	{
//		if(i)r=radius2;
//		for(j=0;j<=slices;j++)
//		{
//			points[i][j].x=r*cos(((double)j/(double)(2*slices))*2*PI);
//			points[i][j].y=r*sin(((double)j/(double)(2*slices))*2*PI);
//			points[i][j].z=h;
//		}
//	}
//
//	for(i=0;i<1;i++)
//	{
//		j=0;
//		glColor3f(0,0,(double)colors/(double)(slices));
//		normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z,points[i+1][j].x,points[i+1][j].y,points[i+1][j].z,points[i+1][j].x,points[i+1][j].y,points[i+1][j].z-2);
//		glBegin(GL_QUADS);{
//				glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z);
//				glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z);
//				glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z-2);
//				glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z-2);
//		}glEnd();
//		j=slices;
//		glColor3f(0,(double)colors/(double)(slices),0);
//		normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z,points[i+1][j].x,points[i+1][j].y,points[i+1][j].z,points[i+1][j].x,points[i+1][j].y,points[i+1][j].z-2);
//		glBegin(GL_QUADS);{
//				glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z);
//				glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z);
//				glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z-2);
//				glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z-2);
//		}glEnd();
//		for(j=0;j<slices;j++)
//		{
//			glColor3f(1,1,1);
//			glEnable(GL_TEXTURE_2D);
//			glBindTexture(GL_TEXTURE_2D,tex4);
//			normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z,points[i][j+1].x,points[i][j+1].y,points[i][j+1].z,points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//			glBegin(GL_QUADS);{
//				glTexCoord2f(0,0);glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z);
//				glTexCoord2f(1,0);glVertex3f(points[i][j+1].x,points[i][j+1].y,points[i][j+1].z);
//				glTexCoord2f(1,1);glVertex3f(points[i+1][j+1].x,points[i+1][j+1].y,points[i+1][j+1].z);
//				glTexCoord2f(0,1);glVertex3f(points[i+1][j].x,points[i+1][j].y,points[i+1][j].z);
//			}glEnd();
//			glDisable(GL_TEXTURE_2D);
//			glColor3f(.1,.1,.1);
//			normal_wrapper(points[i][j].x,points[i][j].y,points[i][j].z-2,points[i][j+1].x,points[i][j+1].y,points[i][j+1].z-2,0,0,height-2);
//			glBegin(GL_TRIANGLES);{
//				glVertex3f(points[i][j].x,points[i][j].y,points[i][j].z-2);
//				glVertex3f(points[i][j+1].x,points[i][j+1].y,points[i][j+1].z-2);
//				glVertex3f(0,0,height-2);
//			}glEnd();
//
//		}
//	}
//}
//
//
//void draw_lower(){
//	draw_lower_sides();
//	glPushMatrix();
//		glTranslatef(0,11,0);
//		glRotatef(90,0,0,1);
//		draw_lower_half_cylinder_with_boat(11,12,17,tex3);
//		draw_lower_half_cover(10,11,12,19,12);
//		glTranslatef(0,0,17);
//		draw_lower_half_cylinder(11,12,2,tex4);
//		draw_lower_half_cylinder(10,12,2,tex4);
//	glPopMatrix();
//	glPushMatrix();
//		glTranslatef(20,11,0);
//		glRotatef(-90,0,0,1);
//		draw_lower_half_cylinder_with_boat(11,12,17,tex3);
//		draw_lower_half_cover(10,11,12,19,12);
//		glTranslatef(0,0,17);
//		draw_lower_half_cylinder(11,12,2,tex4);
//		draw_lower_half_cylinder(10,12,2,tex4);
//	glPopMatrix();
//
//}
//
//void draw_pol(double ppoint1[][2],double xdist,int sz){
//	for(int j=0;j<2;j++){
//		normal_wrapper(j*xdist,ppoint1[0][0],ppoint1[0][1],j*xdist,ppoint1[1][0],ppoint1[1][1],j*xdist,ppoint1[2][0],ppoint1[2][1]);
//		glBegin(GL_POLYGON);
//		for (int i = 0; i <sz; ++i)
//		{
//			glVertex3f(j*xdist,ppoint1[i][0],ppoint1[i][1]);
//		}
//		glEnd();
//	}
//	
//	for (int i = 0; i <sz; ++i)
//	{
//		normal_wrapper(0,ppoint1[i][0],ppoint1[i][1],0,ppoint1[(i+1)%sz][0],ppoint1[(i+1)%sz][1],xdist,ppoint1[(i+1)%sz][0],ppoint1[(i+1)%sz][1]);
//		glBegin(GL_QUADS);
//		glVertex3f(0,ppoint1[i][0],ppoint1[i][1]);
//		glVertex3f(0,ppoint1[(i+1)%sz][0],ppoint1[(i+1)%sz][1]);
//		glVertex3f(xdist,ppoint1[(i+1)%sz][0],ppoint1[(i+1)%sz][1]);
//		glVertex3f(xdist,ppoint1[i][0],ppoint1[i][1]);
//		glEnd();
//	}
//	
//}
//
//void draw_pol1(double ppoint1[][2],double xdist,int sz,double ydist){
//	draw_pol(ppoint1,xdist,sz);
//	for (int i = 0; i < sz; ++i)
//	{
//		ppoint1[i][0]=ydist-ppoint1[i][0];
//	}
//	draw_pol(ppoint1,xdist,sz);
//}
//
//
//// crucifix
//void upper_crucifix(double pos[2][3],double ydist){
//	ydist*=2;
//	double xdist=pos[1][0]-pos[0][0];
//	double zdist=1;
//	double top1_y=0.3,top1_z=zdist+.5;
//	glPushMatrix();
//		glColor3f(.1,.7,.5);
//		glTranslatef(pos[0][0],pos[0][1],pos[0][2]);
//		double ppos1[8][3]={{0,0,0},{xdist,0,0},{xdist,ydist,0},{0,ydist,0},
//			{0,0,zdist},{xdist,0,zdist},{xdist,ydist,zdist},{0,ydist,zdist}
//		};
//		//floor
//		draw_qboid(ppos1);
//		glColor3f(1.0,1.0,.4);
//		double ppos2[8][3]={{0,top1_y,top1_z},{xdist,top1_y,top1_z},{xdist,ydist-top1_y,top1_z},{0,ydist-top1_y,top1_z},
//			{0,top1_y,zdist},{xdist,top1_y,zdist},{xdist,ydist-top1_y,zdist},{0,ydist-top1_y,zdist}
//		};
//		//first
//		draw_qboid(ppos2);
//		top1_y+=.3;
//		for (int i = 4; i < 8; ++i){
//			ppos2[i][2]=top1_z+.3;
//			if(i<6)ppos2[i][1]=top1_y;
//			else ppos2[i][1]=ydist-top1_y;
//		}
//		//2nd
//		draw_qboid(ppos2);
//
//		// moajhe
//		//glColor3f(0,1.0,.4);
//		double x_pos=0;
//		ppos2[4][1]+=.2;
//		double ppoint1[5][2]={{ppos2[4][1]-.2,ppos2[4][2]},
//			{ppos2[4][1]+.6,ppos2[4][2]},
//			{ppos2[4][1]+.7,ppos2[4][2]+.2},
//			{ppos2[4][1]+.6,ppos2[4][2]+.4},
//			{ppos2[4][1]+.4,ppos2[4][2]+.4},
//		};
//		draw_pol1(ppoint1,xdist,5,ydist);
//		//glColor3f(1.00,0,.4);
//
//		double start[2]={ppos2[4][1]+.4,ppos2[4][2]+.4};
//		double ppoint7[8][2]={{start[0],start[1]+.1},
//			{start[0]-.2,start[1]+.1},
//			{start[0]-.2,start[1]+.2},
//			{start[0]-.1,start[1]+.25},
//			{start[0]+.05,start[1]+.25},
//			{start[0]+.05,start[1]+.2},
//			{start[0]+.2,start[1]+.2},
//			{start[0],start[1]},
//		};
//		draw_pol1(ppoint7,xdist,8,ydist);
//
//		start[0]=ppos2[4][1]+.9;
//		start[1]=ppos2[4][2]+.8;
//		double ppoint8[8][2]={{start[0],start[1]+.1},
//			{start[0]-.2,start[1]+.1},
//			{start[0]-.2,start[1]+.2},
//			{start[0]-.1,start[1]+.25},
//			{start[0]+.05,start[1]+.25},
//			{start[0]+.05,start[1]+.2},
//			{start[0]+.2,start[1]+.2},
//			{start[0],start[1]},
//		};
//		draw_pol1(ppoint8,xdist,8,ydist);
//
//		double ppoint2[6][2]={
//			{ppos2[4][1]+.6,ppos2[4][2]+.4},
//			{ppos2[4][1]+.4,ppos2[4][2]+.4},
//			{ppos2[4][1]+.9,ppos2[4][2]+.9},
//			{ppos2[4][1]+.9,ppos2[4][2]+.8},
//			{ppos2[4][1]+.8,ppos2[4][2]+.6},
//			{ppos2[4][1]+.76,ppos2[4][2]+.42},
//		};
//		draw_pol1(ppoint2,xdist,6,ydist);
//
//		//glColor3f(.5,.5,.2);
//		double ppoint3[6][2]={
//			{ppos2[4][1]+.9,ppos2[4][2]+.8},
//			{ppos2[4][1]+.8,ppos2[4][2]+.6},
//			{ppos2[4][1]+.84,ppos2[4][2]+.42},
//			{ppos2[4][1]+1,ppos2[4][2]+.4},
//			{ppos2[4][1]+1,ppos2[4][2]+.7},
//		};
//		draw_pol1(ppoint3,xdist,5,ydist);
//
//		//glColor3f(.3,.5,.1);
//		double ppoint4[3][2]={
//			{ppos2[4][1]+1,ppos2[4][2]+.4},
//			{ppos2[4][1]+.9,ppos2[4][2]+.2},
//			{ppos2[4][1]+1,ppos2[4][2]+.0},
//		};
//		draw_pol1(ppoint4,xdist,3,ydist);
//
//		//majher lombo
//		double cross_h=4;
//		double ppoint5[8][3]={
//			{0,ppos2[4][1]+1,zdist},{xdist,ppos2[4][1]+1,zdist},{xdist,ppos2[4][1]+1,cross_h},{0,ppos2[4][1]+1,cross_h},
//			{0,ydist-(ppos2[4][1]+1),zdist},{xdist,ydist-(ppos2[4][1]+1),zdist},{xdist,ydist-(ppos2[4][1]+1),cross_h},{0,ydist-(ppos2[4][1]+1),cross_h},
//		};
//		draw_qboid(ppoint5);
//		cross_h=3.5;
//		double cross_y=1.1;
//		double ppoint6[8][3]={
//			{0,cross_y,cross_h-.1},{xdist,cross_y,cross_h-.1},{xdist,cross_y,cross_h},{0,cross_y,cross_h},
//			{0,ydist-cross_y,cross_h-.1},{xdist,ydist-cross_y,cross_h-.1},{xdist,ydist-cross_y,cross_h},{0,ydist-cross_y,cross_h},
//		};
//		draw_qboid(ppoint6);
//
//		// lombo
//		ppos2[4][1]-=.2;
//		glColor3f(1.0,1.0,.4);
//		ppos2[6][1]=ppos2[7][1]=ppos2[5][1]+.3;
//		for (int i = 0; i < 4; ++i){
//			ppos2[i][0]=ppos2[i+4][0];
//			ppos2[i][1]=ppos2[i+4][1];
//			ppos2[i][2]=ppos2[i+4][2]+.7;
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//		for (int i = 0; i < 4; ++i){
//			if(i<2)ppos2[i][1]+=.03;
//			else ppos2[i][1]-=.03;
//			ppos2[i+4][0]=ppos2[i][0];
//			ppos2[i+4][1]=ppos2[i][1];
//			ppos2[i+4][2]=ppos2[i][2]+.3;
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//
//		for (int i = 0; i < 4; ++i){
//			if(i<2)ppos2[i+4][1]-=.03;
//			else ppos2[i+4][1]+=.03;
//			ppos2[i][0]=ppos2[i+4][0];
//			ppos2[i][1]=ppos2[i+4][1];
//			ppos2[i][2]=ppos2[i+4][2]+.1;
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//		draw_qboid(ppos2);
//		for (int i = 0; i < 8; ++i){
//			ppos2[i][1]=ydist-ppos2[i][1];
//		}
//
//	glPopMatrix();
//}
//
////default size
//void draw_upper_side(int now,int it,double w=146,double h=573,double w2=146){
//	int no_of_points=14;
//	//points generated based on image
//	double textcoord[14][2]={{0,0},{0,260},{10,260},{10,480},{42,480},{42,500},{43,505},{46,510},
//						{46,522},{45,526},{48,530},{48,537},{52,537},{73,573}};
//	double dist_y1=textcoord[3][0]*w2/146.0;
//	double points[14][2]={{0,0},{0,260},{10,260},{10,480},{42,480},{42,500},{43,505},{46,510},
//						{46,522},{45,526},{48,530},{48,537},{52,537},{73,573}};
//	double tops[]={71,75};
//	double dist_y2[]={58,88};
//
//	double door[7][2]={{30,0},{30,68},{64,98},{86,98},{120,68},{120,0}};
//	double door_pos_w=30*w/146,door_pos_h=98*h/573,add=116*w/146;
//	double door_w=34,door_h=30;
//	double cen[2][2]={{64*w/146,68*h/573},{82*w/146,68*h/573}};
//
//	double extra_x[]={46,100};
//	double extra_y[]={373,389,423};
//
//	double extra_surf[4][2]={{43,245},{103,245},{103,285},{43,285}};
//	double extra_point[4][3]={{47,210,52},{59,230,62},{87,230,84},{99,210,94}};
//
//	door_w*=w/146,door_h*=h/573;
//	if(it%2==0){
//		points[0][1]=98;textcoord[0][1]=98;
//	}
//	if(w>w2){
//		tops[0]=58,tops[1]=88;
//		dist_y2[0]=71,dist_y2[1]=75;
//	}
//	for (int i = 0; i < no_of_points; ++i)
//	{
//		points[i][0]*=w/146.0;
//		points[i][1]*=h/573.0;
//		if(i<2){
//			tops[i]*=w/146.0;
//			dist_y2[i]*=w2/146.0;
//			extra_x[i]*=w/146.0;
//		}
//		if(i<3)extra_y[i]*=h/573.0;
//		if(i<4){
//			extra_surf[i][0]*=w/146.0;
//			extra_surf[i][1]*=h/573.0;
//			extra_point[i][0]*=w/146.0;
//			extra_point[i][1]*=h/573.0;
//			extra_point[i][2]*=w/146.0;
//		}
//		if(i<6){
//			door[i][0]*=w/146.0;
//			door[i][1]*=h/573.0;
//		}
//	}
//	double distance[]={0,0,0,2.50,3.73,3.84,4.2,4.86,4.95,5.22,5.56,7.45,7.45};
//
//	//bridge upper
//	glPushMatrix();
//		
//	if(it==0&&now==1){
//		glTranslatef(0,-54,42);
//		draw_side(2,54.0,2,1,tex6);
//		glTranslatef(14,0,0);
//		draw_side(2,54.0,2,1,tex6);
//	}
//		
//	glPopMatrix();
//
//	glPushMatrix();
//		glColor3f(1,1,1);
//		glEnable(GL_TEXTURE_2D);
//		if(it%2)glBindTexture(GL_TEXTURE_2D,tex1);
//		else glBindTexture(GL_TEXTURE_2D,tex5);
//		//polygon 1
//		normal_y();
//		glBegin(GL_POLYGON);
//		for (int i = 0; i<1 ; ++i)
//		{
//			glTexCoord2f(.5,textcoord[i][1]/573.0);
//			glVertex3f(w/20,0,points[i][1]/10);
//			break;
//		}
//		for (int i = no_of_points-1; i>=0 ; --i)
//		{
//			glTexCoord2f((146-textcoord[i][0])/146.0,textcoord[i][1]/573.0);
//			glVertex3f((w-points[i][0])/10,0,points[i][1]/10);
//		}
//		glEnd();
//
//		//polygon 2
//		normal_y();
//		glBegin(GL_POLYGON);
//		for (int i = 0; i>=0 ; --i)
//		{
//			glTexCoord2f(.5,textcoord[i][1]/573.0);
//			glVertex3f(w/20,0,points[i][1]/10);
//			break;
//		}
//		for (int i = 0; i < no_of_points; ++i)
//		{
//			glTexCoord2f(textcoord[i][0]/146.0,textcoord[i][1]/573.0);
//			glVertex3f(points[i][0]/10,0,points[i][1]/10);
//		}
//		
//		glEnd();
//
//		// extra
//		double hheight=-1.5;
//		
//		for (int i = 0; i < 2; ++i)
//		{
//
//			//normal_wrapper(extra_x[0]/10,i*hheight,extra_y[i]/10,extra_x[1]/10,i*hheight,extra_y[i]/10,extra_x[1]/10,hheight,extra_y[i+1]/10);
//			glBegin(GL_QUADS);
//			glTexCoord2f(extra_x[0]/w,extra_y[i]/h);
//			glVertex3f(extra_x[0]/10,i*hheight,extra_y[i]/10);
//			glTexCoord2f(extra_x[1]/w,extra_y[i]/h);
//			glVertex3f(extra_x[1]/10,i*hheight,extra_y[i]/10);
//			glTexCoord2f(extra_x[1]/w,extra_y[i+1]/h);
//			glVertex3f(extra_x[1]/10,hheight,extra_y[i+1]/10);
//			glTexCoord2f(extra_x[0]/w,extra_y[i+1]/h);
//			glVertex3f(extra_x[0]/10,hheight,extra_y[i+1]/10);
//			glEnd();
//
//			//normal_wrapper(extra_x[i]/10,0,extra_y[0]/10,extra_x[i]/10,hheight,extra_y[1]/10,extra_x[i]/10,hheight,extra_y[2]/10);
//			glBegin(GL_QUADS);
//			glTexCoord2f(extra_x[0]/w,(extra_y[0]-20)/h);
//			glVertex3f(extra_x[i]/10,0,extra_y[0]/10);
//			glTexCoord2f(extra_x[0]/w,(extra_y[0]-10)/h);
//			glVertex3f(extra_x[i]/10,hheight,extra_y[1]/10);
//			glTexCoord2f(extra_x[1]/w,(extra_y[0]-10)/h);
//			glVertex3f(extra_x[i]/10,hheight,extra_y[2]/10);
//			glTexCoord2f(extra_x[1]/w,(extra_y[0]-20)/h);
//			glVertex3f(extra_x[i]/10,0,extra_y[2]/10);
//			glEnd();
//		}
//
//		//normal_wrapper(extra_x[0]/10,hheight,extra_y[2]/10,extra_x[0]/10,0,extra_y[2]/10,extra_x[1]/10,0,extra_y[2]/10);
//		glBegin(GL_QUADS);
//		glTexCoord2f(extra_x[0]/w,(extra_y[0]-20)/h);
//		glVertex3f(extra_x[0]/10,hheight,extra_y[2]/10);
//		glTexCoord2f(extra_x[0]/w,(extra_y[0]-10)/h);
//		glVertex3f(extra_x[0]/10,0,extra_y[2]/10);
//		glTexCoord2f(extra_x[1]/w,(extra_y[0]-10)/h);
//		glVertex3f(extra_x[1]/10,0,extra_y[2]/10);
//		glTexCoord2f(extra_x[1]/w,(extra_y[0]-20)/h);
//		glVertex3f(extra_x[1]/10,hheight,extra_y[2]/10);
//
//		glEnd();
//
//
//		if(it%2==0){
//
//			//extra 2
//			
//			for (int i = 0; i < 4; ++i)
//			{
//				//normal_wrapper(extra_surf[i][0]/10,0,extra_surf[i][1]/10,extra_surf[i][0]/10,hheight,extra_surf[i][1]/10,extra_surf[(i+1)%4][0]/10,hheight,extra_surf[(i+1)%4][1]/10);
//				glBegin(GL_QUADS);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-20)/h);
//				glVertex3f(extra_surf[i][0]/10,0,extra_surf[i][1]/10);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_surf[i][0]/10,hheight,extra_surf[i][1]/10);
//				glTexCoord2f(extra_x[1]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_surf[(i+1)%4][0]/10,hheight,extra_surf[(i+1)%4][1]/10);
//				glTexCoord2f(extra_x[1]/w,(extra_y[0]-20)/h);
//				glVertex3f(extra_surf[(i+1)%4][0]/10,0,extra_surf[(i+1)%4][1]/10);
//				glEnd();
//
//
//				//normal_wrapper(extra_point[i][0]/10,0,extra_point[i][1]/10,extra_point[i][2]/10,0,extra_point[i][1]/10,extra_point[i][2]/10,hheight,extra_surf[0][1]/10);
//				glBegin(GL_QUADS);
//				glTexCoord2f(extra_point[i][0]/w,extra_point[i][1]/h);
//				glVertex3f(extra_point[i][0]/10,0,extra_point[i][1]/10);
//				glTexCoord2f(extra_point[i][2]/w,extra_point[i][1]/h);
//				glVertex3f(extra_point[i][2]/10,0,extra_point[i][1]/10);
//				glTexCoord2f(extra_point[i][2]/w,extra_surf[0][1]/h);
//				glVertex3f(extra_point[i][2]/10,hheight,extra_surf[0][1]/10);
//				glTexCoord2f(extra_point[i][0]/w,extra_surf[0][1]/h);
//				glVertex3f(extra_point[i][0]/10,hheight,extra_surf[0][1]/10);
//				glEnd();
//
//				//normal_wrapper(extra_point[i][0]/10,0,extra_point[i][1]/10,extra_point[i][0]/10,0,extra_surf[0][1]/10,extra_point[i][0]/10,hheight,extra_surf[0][1]/10);
//				glBegin(GL_TRIANGLES);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-20)/h);
//				glVertex3f(extra_point[i][0]/10,0,extra_point[i][1]/10);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_point[i][0]/10,0,extra_surf[0][1]/10);
//				glTexCoord2f(extra_x[1]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_point[i][0]/10,hheight,extra_surf[0][1]/10);
//
//				glEnd();
//
//				//normal_wrapper(extra_point[i][2]/10,0,extra_point[i][1]/10,extra_point[i][2]/10,0,extra_surf[0][1]/10,extra_point[i][2]/10,hheight,extra_surf[0][1]/10);
//				glBegin(GL_TRIANGLES);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-20)/h);
//				glVertex3f(extra_point[i][2]/10,0,extra_point[i][1]/10);
//				glTexCoord2f(extra_x[0]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_point[i][2]/10,0,extra_surf[0][1]/10);
//				glTexCoord2f(extra_x[1]/w,(extra_y[0]-10)/h);
//				glVertex3f(extra_point[i][2]/10,hheight,extra_surf[0][1]/10);
//				glEnd();
//
//			}
//
//			//normal_wrapper(extra_surf[0][0]/10,hheight,extra_surf[0][1]/10,extra_surf[1][0]/10,hheight,extra_surf[1][1]/10,extra_surf[2][0]/10,hheight,extra_surf[2][1]/10);
//			glBegin(GL_QUADS);
//			for (int i = 0; i < 4; ++i)
//			{
//				glTexCoord2f(extra_surf[i][0]/w,extra_surf[i][1]/h);
//				glVertex3f(extra_surf[i][0]/10,hheight,extra_surf[i][1]/10);
//			}
//			glEnd();
//
//
//			// door
//			//normal_wrapper(0,0,0,door_pos_w/10,0,0,door_pos_w/10,0,door_pos_h/10);
//			glBegin(GL_QUADS);
//			glTexCoord2f(0,0);glVertex3f(0,0,0);
//			glTexCoord2f(door_pos_w/w,0);glVertex3f(door_pos_w/10,0,0);
//			glTexCoord2f(door_pos_w/w,door_pos_h/h);glVertex3f(door_pos_w/10,0,door_pos_h/10);
//			glTexCoord2f(0,door_pos_h/h);glVertex3f(0,0,door_pos_h/10);
//			glEnd();
//
//			//normal_wrapper(add/10,0,0,(w)/10,0,0,(w)/10,0,door_pos_h/10);
//			glBegin(GL_QUADS);
//			glTexCoord2f(add/w,0);glVertex3f(add/10,0,0);
//			glTexCoord2f(1,0);glVertex3f((w)/10,0,0);
//			glTexCoord2f(1,door_pos_h/h);glVertex3f((w)/10,0,door_pos_h/10);
//			glTexCoord2f(add/w,door_pos_h/h);glVertex3f(add/10,0,door_pos_h/10);
//			glEnd();
//			double slice=25,pos1[30],pos2[30];
//			for (int i = 0; i <=slice; ++i)
//			{
//				pos1[i]=door_w*cos((double)(i)/slice*(PI/2));
//				pos2[i]=door_h*sin((double)(i)/slice*(PI/2));
//				if(i==0)continue;
//				//normal_wrapper(door_pos_w/10,0,door_pos_h/10,(cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10,(cen[0][0]-pos1[i-1])/10,0,(cen[0][1]+pos2[i-1])/10);
//				glBegin(GL_TRIANGLES);
//				glTexCoord2f(door_pos_w/w,door_pos_h/h);glVertex3f(door_pos_w/10,0,door_pos_h/10);
//				glTexCoord2f((cen[0][0]-pos1[i])/w,(cen[0][1]+pos2[i])/h);glVertex3f((cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10);
//				glTexCoord2f((cen[0][0]-pos1[i-1])/w,(cen[0][1]+pos2[i-1])/h);glVertex3f((cen[0][0]-pos1[i-1])/10,0,(cen[0][1]+pos2[i-1])/10);
//				glEnd();
//
//				//normal_wrapper(add/10,0,door_pos_h/10,(cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10,(cen[1][0]+pos1[i-1])/10,0,(cen[1][1]+pos2[i-1])/10);
//				glBegin(GL_TRIANGLES);
//				glTexCoord2f(add/w,door_pos_h/h);glVertex3f(add/10,0,door_pos_h/10);
//				glTexCoord2f((cen[1][0]+pos1[i])/w,(cen[1][1]+pos2[i])/h);glVertex3f((cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10);
//				glTexCoord2f((cen[1][0]+pos1[i-1])/w,(cen[1][1]+pos2[i-1])/h);glVertex3f((cen[1][0]+pos1[i-1])/10,0,(cen[1][1]+pos2[i-1])/10);
//				glEnd();
//			}
//		}
//
//		glDisable(GL_TEXTURE_2D);
//
//		if(it==0){
//			glEnable(GL_TEXTURE_2D);
//			glBindTexture(GL_TEXTURE_2D,tex4);
//
//			double slice=25,pos1[30],pos2[30];
//			for (int i = 0; i <=slice; ++i)
//			{
//				pos1[i]=door_w*cos((double)(i)/slice*(PI/2));
//				pos2[i]=door_h*sin((double)(i)/slice*(PI/2));
//				if(i==0)continue;
//				//normal_wrapper((cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10,(cen[0][0]-pos1[i-1])/10,0,(cen[0][1]+pos2[i-1])/10,(cen[0][0]-pos1[i-1])/10,18,(cen[0][1]+pos2[i-1])/10);
//				glBegin(GL_QUADS);
//				glVertex3f((cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10);
//				glVertex3f((cen[0][0]-pos1[i-1])/10,0,(cen[0][1]+pos2[i-1])/10);
//				glVertex3f((cen[0][0]-pos1[i-1])/10,18,(cen[0][1]+pos2[i-1])/10);
//				glVertex3f((cen[0][0]-pos1[i])/10,18,(cen[0][1]+pos2[i])/10);
//				glEnd();
//
//				//normal_wrapper((cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10,(cen[1][0]+pos1[i-1])/10,0,(cen[1][1]+pos2[i-1])/10,(cen[1][0]+pos1[i-1])/10,18,(cen[1][1]+pos2[i-1])/10);
//				glBegin(GL_QUADS);
//				glVertex3f((cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10);
//				glVertex3f((cen[1][0]+pos1[i-1])/10,0,(cen[1][1]+pos2[i-1])/10);
//				glVertex3f((cen[1][0]+pos1[i-1])/10,18,(cen[1][1]+pos2[i-1])/10);
//				glVertex3f((cen[1][0]+pos1[i])/10,18,(cen[1][1]+pos2[i])/10);
//				glEnd();
//
//				//normal_wrapper((cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10,(cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10,(cen[1][0]+pos1[i])/10,18,(cen[1][1]+pos2[i])/10);
//				glBegin(GL_QUADS);
//				if(i==slice){
//					glVertex3f((cen[0][0]-pos1[i])/10,0,(cen[0][1]+pos2[i])/10);
//					glVertex3f((cen[1][0]+pos1[i])/10,0,(cen[1][1]+pos2[i])/10);
//					glVertex3f((cen[1][0]+pos1[i])/10,18,(cen[1][1]+pos2[i])/10);
//					glVertex3f((cen[0][0]-pos1[i])/10,18,(cen[0][1]+pos2[i])/10);
//				}
//				glEnd();
//			}
//
//			//normal_wrapper(door_pos_w/10,0,0,door_pos_w/10,0,door_pos_h/10,door_pos_w/10,18,door_pos_h/10);
//			glBegin(GL_QUADS);
//			glVertex3f(door_pos_w/10,0,0);
//			glVertex3f(door_pos_w/10,0,door_pos_h/10);
//			glVertex3f(door_pos_w/10,18,door_pos_h/10);
//			glVertex3f(door_pos_w/10,18,0);
//			glEnd();
//
//			//normal_wrapper(add/10,0,0,add/10,0,door_pos_h/10,add/10,18,door_pos_h/10);
//			glBegin(GL_QUADS);
//			glVertex3f(add/10,0,0);
//			glVertex3f(add/10,0,door_pos_h/10);
//			glVertex3f(add/10,18,door_pos_h/10);
//			glVertex3f(add/10,18,0);
//			
//			glEnd();
//			glDisable(GL_TEXTURE_2D);
//		}

//		//dom
//		glEnable(GL_TEXTURE_2D);
//		glBindTexture(GL_TEXTURE_2D,tex2);
//		normal_wrapper(tops[0]/10,dist_y2[0]/10,62,points[3][0]/10,dist_y1/10,points[3][1]/10,(w-points[3][0])/10,dist_y1/10,points[3][1]/10);
//		glBegin(GL_QUADS);
//			glVertex3f(points[3][0]/10,dist_y1/10,points[3][1]/10);
//			glVertex3f(tops[0]/10,dist_y2[0]/10,62);
//			glVertex3f(tops[1]/10,dist_y2[0]/10,62);
//			glVertex3f((w-points[3][0])/10,dist_y1/10,points[3][1]/10);
//		glEnd();
//
//		normal_y();
//		// front 
//		for (int i = 3; i < no_of_points-1; ++i)
//		{
//			//normal_wrapper(points[i][0]/10,0,points[i][1]/10,points[i][0]/10,distance[i],points[i][1]/10,points[i+1][0]/10,distance[i],points[i+1][1]/10);
//			glBegin(GL_QUADS);
//			glVertex3f(points[i][0]/10,0,points[i][1]/10);
//			glVertex3f(points[i][0]/10,distance[i],points[i][1]/10);
//			glVertex3f(points[i+1][0]/10,distance[i],points[i+1][1]/10);
//			glVertex3f(points[i+1][0]/10,0,points[i+1][1]/10);
//			glEnd();
//
//			//normal_wrapper((w-points[i][0])/10,0,points[i][1]/10,(w-points[i][0])/10,distance[i],points[i][1]/10,(w-points[i+1][0])/10,distance[i],points[i+1][1]/10);
//			glBegin(GL_QUADS);
//			glVertex3f((w-points[i][0])/10,0,points[i][1]/10);
//			glVertex3f((w-points[i][0])/10,distance[i],points[i][1]/10);
//			glVertex3f((w-points[i+1][0])/10,distance[i],points[i+1][1]/10);
//			glVertex3f((w-points[i+1][0])/10,0,points[i+1][1]/10);
//			glEnd();
//		}
//		glDisable(GL_TEXTURE_2D);
//		// special
//		int i = no_of_points-3;
//
//			glEnable(GL_TEXTURE_2D);
//			glBindTexture(GL_TEXTURE_2D,tex1);
//			//normal_wrapper(points[i][0]/10+.2,0.41,points[i][1]/10,points[i][0]/10+.2,0.41,points[i][1]/10+2.12,points[i][0]/10+.2+1.68,0.41,points[i+1][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glTexCoord2f((textcoord[i][0]+2)/146.0,textcoord[i][1]/573.0);glVertex3f(points[i][0]/10+.2,0.41,points[i][1]/10);
//			glTexCoord2f((textcoord[i][0]+2)/146.0,(textcoord[i][1]+21.2)/573.0);glVertex3f(points[i][0]/10+.2,0.41,points[i][1]/10+2.12);
//			glTexCoord2f((textcoord[i][0]+16.4)/146.0,(textcoord[i+1][1]+21.2)/573.0);glVertex3f(points[i][0]/10+.2+1.68,0.41,points[i+1][1]/10+2.12);
//			glTexCoord2f((textcoord[i+1][0])/146.0,textcoord[i+1][1]/573.0);glVertex3f(points[i+1][0]/10,0.41,points[i+1][1]/10);
//			glEnd();
//			//normal_wrapper(w/10-(points[i][0]/10+.2),0.41,points[i][1]/10,w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12,w/10-(points[i][0]/10+.2+1.68),.41,points[i+1][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glTexCoord2f((textcoord[i][0]+2)/146.0,textcoord[i][1]/573.0);glVertex3f(w/10-(points[i][0]/10+.2),0.41,points[i][1]/10);
//			glTexCoord2f((textcoord[i][0]+2)/146.0,(textcoord[i][1]+21.2)/573.0);glVertex3f(w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12);
//			glTexCoord2f((textcoord[i][0]+16.4)/146.0,(textcoord[i+1][1]+21.2)/573.0);glVertex3f(w/10-(points[i][0]/10+.2+1.68),.41,points[i+1][1]/10+2.12);
//			glTexCoord2f((textcoord[i+1][0])/146.0,textcoord[i+1][1]/573.0);glVertex3f(w/10-(points[i+1][0]/10),.41,points[i+1][1]/10);
//			glEnd();
//			glDisable(GL_TEXTURE_2D);
//
//			glEnable(GL_TEXTURE_2D);
//			glBindTexture(GL_TEXTURE_2D,tex2);
//			
//			//normal_wrapper(points[i][0]/10+.2,0.41,points[i][1]/10,points[i][0]/10+.2,.41,points[i][1]/10+2.12,points[i][0]/10+.2,distance[i],points[i][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glVertex3f(points[i][0]/10+.2,0.41,points[i][1]/10);
//			glVertex3f(points[i][0]/10+.2,.41,points[i][1]/10+2.12);
//			glVertex3f(points[i][0]/10+.2,distance[i],points[i][1]/10+2.12);
//			glVertex3f(points[i][0]/10+.2,distance[i],points[i][1]/10);
//			glEnd();
//
//			//normal_wrapper(points[i][0]/10+.2,.41,points[i][1]/10+2.12,points[i][0]/10+.2+1.68,.41,points[i+1][1]/10+2.12,points[i][0]/10+.2+1.68,distance[i],points[i+1][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glVertex3f(points[i][0]/10+.2,.41,points[i][1]/10+2.12);
//			glVertex3f(points[i][0]/10+.2+1.68,.41,points[i+1][1]/10+2.12);
//			glVertex3f(points[i][0]/10+.2+1.68,distance[i],points[i+1][1]/10+2.12);
//			glVertex3f(points[i][0]/10+.2,distance[i],points[i][1]/10+2.12);
//			glEnd();
//
//			//normal_wrapper(w/10-(points[i][0]/10+.2),0.41,points[i][1]/10,w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12,w/10-(points[i][0]/10+.2),distance[i],points[i][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glVertex3f(w/10-(points[i][0]/10+.2),0.41,points[i][1]/10);
//			glVertex3f(w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12);
//			glVertex3f(w/10-(points[i][0]/10+.2),distance[i],points[i][1]/10+2.12);
//			glVertex3f(w/10-(points[i][0]/10+.2),distance[i],points[i][1]/10);
//			glEnd();
//			//normal_wrapper(w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12,w/10-(points[i][0]/10+.2+1.68),.41,points[i+1][1]/10+2.12,w/10-(points[i][0]/10+.2+1.68),distance[i],points[i+1][1]/10+2.12);
//			glBegin(GL_QUADS);
//			glVertex3f(w/10-(points[i][0]/10+.2),.41,points[i][1]/10+2.12);
//			glVertex3f(w/10-(points[i][0]/10+.2+1.68),.41,points[i+1][1]/10+2.12);
//			glVertex3f(w/10-(points[i][0]/10+.2+1.68),distance[i],points[i+1][1]/10+2.12);
//			glVertex3f(w/10-(points[i][0]/10+.2),distance[i],points[i][1]/10+2.12);
//			
//			glEnd();
//
//			glDisable(GL_TEXTURE_2D);
//		
//	glPopMatrix();
//
//	//call crucifix with 2 points and y distance
//	double pppoint[2][3]={{tops[0]/10,dist_y2[0]/10,62},{tops[1]/10,dist_y2[0]/10,62}};
//	double ydist=(w2/2-dist_y2[0])/10;
//	if(it==0)upper_crucifix(pppoint,ydist);
//
//
//	// cylinder
//	glPushMatrix();
//
//	glColor3f(1,1,1);
//	glEnable(GL_TEXTURE_2D);
//	
//	int lower_slice=24,mid_slice=8;
//	double pos1[30],pos2[30],pos3[30],pos4[30];
//	double rad_lower=1,rad_upper=1.5;
//	double first_hieght=380;
//	for (int i = 0; i <= lower_slice; ++i)
//	{
//		pos1[i]=cos((double)(i)/lower_slice*(PI*2));
//		pos2[i]=sin((double)(i)/lower_slice*(PI*2));
//		if(i==0)continue;
//		glBindTexture(GL_TEXTURE_2D,tex9);
//		normal_wrapper(rad_lower*pos1[i-1],rad_lower*pos2[i-1],0,rad_lower*pos1[i],rad_lower*pos2[i],0,rad_lower*pos1[i],rad_lower*pos2[i],first_hieght/10);
//		glBegin(GL_QUADS);
//		glTexCoord2f(0,0);glVertex3f(rad_lower*pos1[i],rad_lower*pos2[i],0);
//		glTexCoord2f(1,0);glVertex3f(rad_lower*pos1[i-1],rad_lower*pos2[i-1],0);
//		glTexCoord2f(1,1);glVertex3f(rad_lower*pos1[i-1],rad_lower*pos2[i-1],first_hieght/10);
//		glTexCoord2f(0,1);glVertex3f(rad_lower*pos1[i],rad_lower*pos2[i],first_hieght/10);
//		glEnd();
//	}
//
//	for (int i = 0; i <= mid_slice; ++i)
//	{
//		pos3[i]=cos((double)(i)/mid_slice*(PI*2));
//		pos4[i]=sin((double)(i)/mid_slice*(PI*2));
//		if(i==0)continue;
//
//		glBindTexture(GL_TEXTURE_2D,tex10);
//		normal_wrapper(rad_upper*pos3[i-1],rad_upper*pos4[i-1],first_hieght/10,rad_upper*pos3[i],rad_upper*pos4[i],first_hieght/10,rad_upper*pos3[i],rad_upper*pos4[i],points[8][1]/10);
//		glBegin(GL_POLYGON);
//		glTexCoord2f(0,0);glVertex3f(rad_lower*pos1[(i-1)*3],rad_lower*pos2[(i-1)*3],first_hieght/10-2);
//		glTexCoord2f(0.33,.25);glVertex3f(rad_lower*pos1[(i-1)*3+1],rad_lower*pos2[(i-1)*3+1],first_hieght/10-1.5);
//		glTexCoord2f(.66,.25);glVertex3f(rad_lower*pos1[(i-1)*3+2],rad_lower*pos2[(i-1)*3+2],first_hieght/10-1.5);
//		glTexCoord2f(1,0);glVertex3f(rad_lower*pos1[i*3],rad_lower*pos2[i*3],first_hieght/10-2);
//		glTexCoord2f(1,1);glVertex3f(rad_upper*pos3[i],rad_upper*pos4[i],first_hieght/10);
//		glTexCoord2f(0,1);glVertex3f(rad_upper*pos3[i-1],rad_upper*pos4[i-1],first_hieght/10);
//		glEnd();
//
//		glBindTexture(GL_TEXTURE_2D,tex11);
//		normal_wrapper(rad_upper*pos3[i-1],rad_upper*pos4[i-1],first_hieght/10,rad_upper*pos3[i],rad_upper*pos4[i],first_hieght/10,rad_upper*pos3[i],rad_upper*pos4[i],points[8][1]/10);
//		glBegin(GL_QUADS);
//		glTexCoord2f(0,0);glVertex3f(rad_upper*pos3[i-1],rad_upper*pos4[i-1],first_hieght/10);
//		glTexCoord2f(1,0);glVertex3f(rad_upper*pos3[i],rad_upper*pos4[i],first_hieght/10);
//		glTexCoord2f(1,1);glVertex3f(rad_upper*pos3[i],rad_upper*pos4[i],points[8][1]/10);
//		glTexCoord2f(0,1);glVertex3f(rad_upper*pos3[i-1],rad_upper*pos4[i-1],points[8][1]/10);
//		glEnd();
//
//		glBindTexture(GL_TEXTURE_2D,tex12);
//		normal_wrapper(rad_upper*pos3[i-1],rad_upper*pos4[i-1],points[8][1]/10,rad_upper*pos3[i],rad_upper*pos4[i],points[8][1]/10,0,0,62);
//		glBegin(GL_TRIANGLES);
//		glTexCoord2f(0,0);glVertex3f(rad_upper*pos3[i-1],rad_upper*pos4[i-1],points[8][1]/10);
//		glTexCoord2f(1,0);glVertex3f(rad_upper*pos3[i],rad_upper*pos4[i],points[8][1]/10);
//		glTexCoord2f(.5,.6);glVertex3f(0,0,62);
//		glEnd();
//	}
//
//	glDisable(GL_TEXTURE_2D);
//
//	glPopMatrix();
//}
//
//
//void draw_upper(int now=0){
//	double surfaces[]={20,22,17};
//	double unit=2;
//	double co_ords[4][2]={{unit,unit},{surfaces[0]-unit,unit},{surfaces[0]-unit,surfaces[1]-unit},{unit,surfaces[1]-unit}};
//	double width[4]={(surfaces[0]-2*unit)*10,(surfaces[1]-2*unit)*10,(surfaces[0]-2*unit)*10,(surfaces[1]-2*unit)*10};
//
//	for (int i = 0; i < 4; ++i)
//	{
//		glPushMatrix();
//		glTranslatef(co_ords[i][0]-10,co_ords[i][1]+25,surfaces[2]);
//		glRotatef(90*i,0,0,1);
//		draw_upper_side(now,i,width[i],573,width[(i+1)%2]);
//		glPopMatrix();
//	}
//}
//
//

/*============================ Main Draw Function =============================*/

void draw()
{
	
	glPushMatrix();
		glTranslatef(-10,25,0);
		//draw_lower();
	glPopMatrix();
	glPushMatrix();
		//draw_upper(1);
	glPopMatrix();
	

	glPushMatrix();
		glRotatef(180,0,0,1);
		glTranslatef(-10,25,0);
//		draw_lower();
	glPopMatrix();
	glPushMatrix();
		glRotatef(180,0,0,1);
//		draw_upper();
	glPopMatrix();
	

}



void keyboardListener(unsigned char key, int x,int y){
	switch(key){

		case '1':	
			Camera.RotateX(5.0);
			break;
		case '2':	
			Camera.RotateX(-5.0);
			break;
		case '3':	
			Camera.RotateY(5.0);
			break;
		case '4':	
			Camera.RotateY(-5.0);
			break;
		case '5':	
			Camera.RotateZ(5.0);
			break;
		case '6':	
			Camera.RotateZ(-5.0);
			break;
		case '7':	
			Angle += AngleDelta;
			break;
		case '8':	
			Angle -= AngleDelta;
			break;
		case '9':	
			is_blu=1-is_blu;
			break;
		default:
			break;
	}
}


void specialKeyListener(int key, int x,int y){
	switch(key){
		case GLUT_KEY_DOWN:		//down arrow key
			Camera.MoveForward( 3.0 ) ;
			break;
		case GLUT_KEY_UP:		// up arrow key
			Camera.MoveForward( -3.0 ) ;
			break;

		case GLUT_KEY_RIGHT:
			Camera.StrafeRight(3.0);
			break;
		case GLUT_KEY_LEFT:
			Camera.StrafeRight(-3.0);
			break;

		case GLUT_KEY_PAGE_UP:
			Camera.MoveUpward(3.0);
			break;
		case GLUT_KEY_PAGE_DOWN:
			Camera.MoveUpward(-3.0);
			break;

		case GLUT_KEY_INSERT:
			if(angg<45)angg+=3;
			break;

		case GLUT_KEY_HOME:
			if(angg>0)angg-=3;
			break;
		case GLUT_KEY_END:
			break;

		default:
			break;
	}
}


void mouseListener(int button, int state, int x, int y){	//x, y is the x-y of the screen (2D)
	switch(button){
		case GLUT_LEFT_BUTTON:
			if(state == GLUT_DOWN){		// 2 times?? in ONE click? -- solution is checking DOWN or UP
				drawaxes=1-drawaxes;	
			}
			break;

		case GLUT_RIGHT_BUTTON:
			//........
			break;

		case GLUT_MIDDLE_BUTTON:
			//........
			break;

		default:
			break;
	}
}

/*============================ Display function ===========================*/


void display(){
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glClearColor(0,0,0,0);	//color black
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	Camera.Render();
	glMatrixMode(GL_MODELVIEW);

	glScalef(4,4,4);
	// point movable diffuse light
	if(is_blu){
		GLfloat diffusePoint[] = {0, 0, 0.5, 1.0}; //Color (0.5, 0.5, 0.5)
		glLightfv(GL_LIGHT1, GL_DIFFUSE, diffusePoint);
	}else{
		GLfloat diffusePoint[] = {0.5, 0.5, 0.5, 1.0}; //Color (0.5, 0.5, 0.5)
		glLightfv(GL_LIGHT1, GL_DIFFUSE, diffusePoint);
	}
	GLfloat position[] = {-40.0*cos(Angle),-40.0*sin(Angle),80,1.0};
    glLightfv(GL_LIGHT1, GL_POSITION, position);

    GLfloat diffuseDir[] = {0.5, 0.5, 0.5, 1.0}; 
    GLfloat lightDir[] = {-15,0,10, 1.0}; 
    glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseDir);
    glLightfv(GL_LIGHT0, GL_POSITION, lightDir);
    glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 30.0);
    GLfloat spot_direction[] = { 1, 1, 0};
	glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, spot_direction);
	glLightf(GL_LIGHT0, GL_SPOT_EXPONENT, 100.0);
    

    // representative object
    glColor3f(0,0,1);
    GLfloat light_emission[] = {1.0, 1.0, 1.0, 1.0};
	GLfloat unset[]={0,0,0,1};
	glMaterialfv(GL_FRONT, GL_EMISSION, light_emission);
	glPushMatrix();
	glColor3f(0,0,1);
	glTranslatef (-40.0*cos(Angle),-40.0*sin(Angle),80);
	glutSolidSphere(1, 36, 36);
	glPopMatrix();
	//material property
	GLfloat mat_ambient[] = { .4, .4, .4, 1.0 };
	GLfloat mat_diffuse[] = { 1, 1, 1, 1.0 };
	GLfloat mat_specular[] = { 1, 1, 1, 1.0 };
	GLfloat high_shininess[] = { 100.0 };
	glMaterialfv(GL_FRONT, GL_AMBIENT, mat_ambient);
	glMaterialfv(GL_FRONT, GL_DIFFUSE, mat_diffuse);
	glMaterialfv(GL_FRONT, GL_SPECULAR, mat_specular);
	glMaterialfv(GL_FRONT, GL_SHININESS, high_shininess);
	glMaterialfv(GL_FRONT, GL_EMISSION, unset);
	drawAxes();
	drawGrid();
		
		draw();
	glutSwapBuffers();
}

void animate(){
	glutPostRedisplay();
}


void load_text(){
	tex1=LoadBitmap("side.bmp");
	tex2=LoadBitmap("upper.bmp");
	tex3=LoadBitmap("lower.bmp");
	tex4=LoadBitmap("lower_side.bmp");
	tex5=LoadBitmap("withdoor.bmp");
	tex6=LoadBitmap("jinis.bmp");
	tex7=LoadBitmap("rep.bmp");
	tex8=LoadBitmap("road.bmp");

	tex9=LoadBitmap("cyl_lower.bmp");
	tex10=LoadBitmap("cyl_mid.bmp");
	tex11=LoadBitmap("cyl_top_mid.bmp");
	tex12=LoadBitmap("cyl_top.bmp");

	tex13=LoadBitmap("road2.bmp");

}

/*============================ Init Function =============================*/


void init(){
	//load texture
	load_text();
	// light
	Angle = 0;	
	AngleDelta = 0.1;
	glClearColor(0,0,0,0);
	glShadeModel(GL_SMOOTH);
	GLfloat lmodel_ambient[] = { .5, .5, .5, 1.0 };
	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, lmodel_ambient);

	

   	glEnable(GL_COLOR_MATERIAL);
   	glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE);
   	glEnable(GL_NORMALIZE);
    glEnable(GL_LIGHTING);
	glEnable(GL_LIGHT0); 
	glEnable(GL_LIGHT1);
	drawgrid=0;
	drawaxes=1;
	cameraHeight=100.0;
	cameraAngle=1.0;
	angle=0;
	Camera.Move( get3dVector(75, 0, 0 ));
	
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(80,	1,	1,	10000.0);
}

/*============================ Main Function =============================*/

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

	glutKeyboardFunc(keyboardListener);
	glutSpecialFunc(specialKeyListener);
	glutMouseFunc(mouseListener);
	glutMainLoop();		//The main loop of OpenGL
	return 0;
}
