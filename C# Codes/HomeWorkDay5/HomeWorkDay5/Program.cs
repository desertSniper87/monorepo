using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace HomeWorkDay5
{
    class Program
    {
        static void Main(string[] args)
        {
            PsychicMutant Eddie = new PsychicMutant();
            PhysicalMutant Brock = new PhysicalMutant();
            ElementalMutant Lesner = new ElementalMutant();

            Eddie.codeName = "Eddie";
            Brock.codeName = "Brock";
            Lesner.codeName = "Lesner";

            Eddie.level = 10;
            Brock.level = 14;
            Lesner.level = 20;

            Eddie.iq = 119;
            Brock.iq = 105;

            Eddie.usageCount = 14;
            Brock.strength = 34;
            Lesner.region = 34;

            Eddie.dangerQuotientMethod();
            Brock.dangerQuotientMethod();
            Lesner.dangerQuotientMethod();

            Eddie.displayInfo();
            Brock.displayInfo();
            Lesner.displayInfo();

            Console.ReadLine();
        }
    }
}
