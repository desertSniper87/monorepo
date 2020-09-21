using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HomeWorkDay5
{
    class PhysicalMutant : Mutant
    {
        public int iq;
        public int strength;

        public override void dangerQuotientMethod()
        {
            dangerQuotientValue = level * iq * strength;
        }
    }
}
