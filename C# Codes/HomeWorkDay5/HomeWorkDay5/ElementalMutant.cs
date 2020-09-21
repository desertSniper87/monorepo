using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HomeWorkDay5
{
    class ElementalMutant : Mutant
    {
        public int region;

        public override void dangerQuotientMethod()
        {
            dangerQuotientValue = level * region;
        }

    }
}
