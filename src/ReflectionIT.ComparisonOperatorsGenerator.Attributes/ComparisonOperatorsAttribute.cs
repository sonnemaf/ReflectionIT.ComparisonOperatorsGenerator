namespace ReflectionIT.ComparisonOperatorsGenerator.Attributes;

/// <summary>
/// An attribute to indicate that comparison operators should be generated for the target class, struct or record
/// <para>
/// This only works if the <see cref="System.IComparable{T}"/> interface is implemented
/// </para>
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct, AllowMultiple = false, Inherited = false)]
public class ComparisonOperatorsAttribute : Attribute {

    /// <summary>
    /// Gets or sets a value indicating whether the <see cref="System.Numerics.IComparisonOperators{TSelf,TOther,TResult}" /> interface should be implemented.
    /// </summary>
    public bool ImplementIComparisonOperatorsInterface { get; set; }
}
