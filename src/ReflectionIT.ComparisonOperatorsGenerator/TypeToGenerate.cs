namespace ReflectionIT.ComparisonOperatorsGenerator;

public readonly struct TypeToGenerate {

    public readonly string Name;
    public readonly string? Namespace;
    public readonly bool ImplementIComparisonOperatorsInterface;
    public readonly bool IsValueType;
    public readonly bool IsRecord;

    public TypeToGenerate(string name, string? @namespace, bool implementIComparisonOperatorsInterface, bool isValueType, bool isRecord) {
        Name = name;
        Namespace = @namespace;
        ImplementIComparisonOperatorsInterface = implementIComparisonOperatorsInterface;
        IsValueType = isValueType;
        IsRecord = isRecord;
    }

    public string FullName => string.IsNullOrEmpty(Namespace) ? Name : $"{Namespace}.{Name}";
}