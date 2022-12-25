using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDemos
{
    public class Demo03
    {
        public static void Demo()
        {
            var rectangle = new Rectange(new Point(1, 0), 20, 10);
            var squre = new Squre(new Point(0, 0), 10);
            var circle = new Circle(new Point(0, 2), 10);

            var shapes = new List<Shape>() { rectangle, squre, circle };

            var totalArea = 0.0;
            foreach (var shape in shapes)
            {
                totalArea += shape.Area;
            }
            Console.WriteLine(totalArea);
        }
    }

    public abstract class Shape
    {
        public abstract float Area { get; }
    }

    public class Point
    {
        public int x; 
        public int y;
        public Point (int x, int y)
        {
            this.x = x;
            this.y = y;
        }
    }

    public class Rectange: Shape
    {
        public Point Point;
        public int Width;
        public int Height;

        public Rectange(Point Point, int Width, int Height)
        {
            this.Width= Width;
            this.Height = Height;
            this.Point = Point;
        }

        public override float Area => this.Width* this.Height;
    }

    public class Squre: Shape
    {
        public Point Point;
        public int Edge;

        public Squre(Point Point, int Edge)
        {
            this.Point = Point; 
            this.Edge = Edge;
        }
        public override float Area => this.Edge * this.Edge;

    }

    public class Circle: Shape
    {
        public Point Point; 
        public int Radius;
        
        public Circle(Point Point, int Radius)
        {
            this.Point = Point;
            this.Radius = Radius;
        }

        public override float Area => (float) 3.14 * this.Radius * this.Radius;
    }
}
