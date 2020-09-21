  #include <iostream>
  using namespace std;

  class area
  {
      double dim1;
      double dim2;
      public:
        virtual double getarea ()
        {
            cout<< "This method must be overridden"<< endl;
        }
        void setdim ( double d1, double d2 )
        {
            dim1 = d1;
            dim2 = d2;
        }
        void getdim ( double &d1, double &d2 )
        {
            d1 = dim1;
            d2 = dim2;
        }
  };

  class rectangle: public area
  {
        double getarea ()
        {
            double d1, d2;

            getdim ( d1, d2 );
            return d1 * d2;
        }
  };

  class triangle: public area
  {
        double getarea ()
        {
            double d1, d2;

            getdim ( d1,d2 );
            return 0.5*d1*d2;
        }
  };

  int main ()
  {
      area *a;

      rectangle r;
      triangle t;

      r.setdim( 11.2,24.5 );
      t.setdim( 20.1,32.2 );

      a = &r;
      cout<<"If rectangle, area = "<< a->getarea()<< endl;

      a = &t;
      cout<<"If triangle, area = "<< a->getarea()<< endl;

      return 0;
  }
