using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HomeWorkDay5
{
    class PsychicMutant : Mutant
    {
        public int iq;
        public int usageCount;

        public override void dangerQuotientMethod()
        {
            dangerQuotientValue = level * usageCount;
        }
    }
}
