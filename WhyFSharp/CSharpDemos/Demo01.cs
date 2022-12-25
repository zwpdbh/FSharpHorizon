using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDemos
{
    public static class Demo01
    {
        // For completeness, this is two methods to ensure that the null check 
        // is done eagerly while the loop is done lazily. If that's not an issue, 
        // you can forego the check and just use the main function.
        public static IEnumerable<T> NonConsecutive<T>(this IEnumerable<T> input)
        {
            if (input == null) throw new ArgumentNullException("input");
            return NonConsecutiveImpl(input);
        }

        static IEnumerable<T> NonConsecutiveImpl<T>(this IEnumerable<T> input)
        {
            bool isFirst = true;
            T last = default(T);
            foreach (var item in input)
            {
                if (isFirst || !object.Equals(item, last))
                {
                    yield return item;
                    last = item;
                    isFirst = false;
                }
            }
        }

        public static void Demo()
        {
            var input01 = new List<int>() { 1, 1, 2, 3, 3, 4, 4, 4 };
            foreach (int i in NonConsecutive<int>(input01))
            {
                Console.WriteLine(i);
            }

            Console.WriteLine();
            var input02 = new List<string>() { "a", "a", "b", "b", "c", "c", "c", "a" };
            foreach (string i in NonConsecutive<string>(input02))
            {
                Console.WriteLine(i);
            }
        }
    }
}
