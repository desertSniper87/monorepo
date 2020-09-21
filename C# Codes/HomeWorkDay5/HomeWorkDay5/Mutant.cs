using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HomeWorkDay5
{
    abstract class Mutant : IDisplay
    {
        #region Properties

        public string codeName;
        public int level;
        public int dangerQuotientValue = 0;

        #endregion

        #region Methods

        public abstract void dangerQuotientMethod();
        

        public void displayInfo ()
        {
            Console.WriteLine( codeName + "-DQ:" + dangerQuotientValue.ToString() );
        }

        #endregion 
    }
}
