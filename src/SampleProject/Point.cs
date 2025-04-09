using ReflectionIT.ComparisonOperatorsGenerator.Attributes;

namespace SampleProject;

[ComparisonOperators]
partial class Point : IComparable<Point> {

    public readonly double X;
    public readonly double Y;

    public Point(double x, double y) {
        this.X = x;
        this.Y = y;
    }

    public void Swap() => new Point(this.Y, this.X);

    public double Dist => Math.Sqrt((X * X) + (Y * Y));

    public override string ToString() => $"({X},{Y})";

    public int CompareTo(Point? other) {
        return Comparer<double?>.Default.Compare(this.Dist, other?.Dist);
    }
}

