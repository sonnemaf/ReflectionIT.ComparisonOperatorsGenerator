using ReflectionIT.ComparisonOperatorsGenerator.Attributes;

//namespace SampleProject;

[ComparisonOperators(ImplementIComparisonOperatorsInterface = true)]
readonly partial record struct Time : IComparable<Time> {

    public readonly int TotalMinutes; 

    public int Hours => TotalMinutes / 60;
    public int Minutes => TotalMinutes % 60;
    public Time(int totalMinutes) {
        ArgumentOutOfRangeException.ThrowIfNegative(totalMinutes);
        TotalMinutes = totalMinutes;
    }

    public Time(int hours, int minutes) : this(hours * 60 + minutes) {
    }

    public override string ToString() => $"{this.Hours}:{this.Minutes:00}";

    public int CompareTo(Time other) => this.TotalMinutes.CompareTo(other.TotalMinutes);
}

