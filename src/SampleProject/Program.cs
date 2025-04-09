using SampleProject;

var p1 = new Point(1, 2);
var p2 = new Point(1, 3);

Console.WriteLine(p1.CompareTo(p2));
Console.WriteLine(p1 > p2);
Console.WriteLine(p1 >= p2);
Console.WriteLine(p1 < p2);
Console.WriteLine(p1 <= p2);

Console.WriteLine();

var t1 = new Time(90);
var t2 = new Time(1, 30);

Console.WriteLine(t1.CompareTo(t2));
Console.WriteLine(t1 > t2);
Console.WriteLine(t1 >= t2);
Console.WriteLine(t1 < t2);
Console.WriteLine(t1 <= t2);

