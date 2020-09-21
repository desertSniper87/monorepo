public class Test
{
	class A {
	        int i;
	
	        A() {
	            ++i;
	        }
	
	        protected int get() {
	            return ++i;
	        }
	    }
	
	    class B extends A {
	        B() {
	            i++;
	        }
	
	        protected int get() {
	            return (i + 3);
	        }
	    }
	
	    class Main extends B {
	        public static void main(String ka[]) {
	            Main obj = new Main();
	            A ob_a = new A();
	            ob_a = (A) obj;
	            System.out.println(ob_a.get());
	        }
	    }
}
