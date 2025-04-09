using System.Collections.Immutable;
using Microsoft.CodeAnalysis;

namespace ReflectionIT.ComparisonOperatorsGenerator;

public static class Extensions
{
    internal static bool DoesImplementInterfaces(this ITypeSymbol type, params string[] interfaces) =>
        type.AllInterfaces.Any(i => interfaces.Contains(i.ToString()));

    internal static bool DoesImplementIComparable(this ITypeSymbol type) =>
        type.DoesImplementInterfaces("System.IComparable");

    internal static bool DoesImplementIComparableOfT(this ITypeSymbol type) =>
        type.DoesImplementInterfaces("System.IComparable<T>");

    internal static bool DoesImplementIComparisonOperators(this ITypeSymbol type) =>
        type.DoesImplementInterfaces("System.IComparisonOperators");
}
