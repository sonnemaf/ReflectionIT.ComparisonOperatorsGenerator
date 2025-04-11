using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using ReflectionIT.ComparisonOperatorsGenerator.Attributes;

namespace ReflectionIT.ComparisonOperatorsGenerator;

[Generator]
public class DisposableGenerator : IIncrementalGenerator {

    public void Initialize(IncrementalGeneratorInitializationContext context) {

        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            "ComparisonOperatorsGenerator.Attributes.g.cs", SourceText.From(SourceGenerationHelper.Attribute, Encoding.UTF8)));

        // Classes
        IncrementalValuesProvider<ClassDeclarationSyntax> classDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForClassGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForClassGeneration(ctx))
            .Where(static m => m is not null)!;

        IncrementalValueProvider<(Compilation, ImmutableArray<ClassDeclarationSyntax>)> compilationAndClasses
            = context.CompilationProvider.Combine(classDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndClasses,
            static (spc, source) => Execute(source.Item1, source.Item2, spc));

        // Structs
        IncrementalValuesProvider<StructDeclarationSyntax> structDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForStructGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForStructGeneration(ctx))
            .Where(static m => m is not null)!;

        IncrementalValueProvider<(Compilation, ImmutableArray<StructDeclarationSyntax>)> compilationAndStructs
            = context.CompilationProvider.Combine(structDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndStructs,
            static (spc, source) => Execute(source.Item1, source.Item2, spc));

        // Records
        IncrementalValuesProvider<RecordDeclarationSyntax> recordDeclarations = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: static (s, _) => IsSyntaxTargetForRecordGeneration(s),
                transform: static (ctx, _) => GetSemanticTargetForRecordGeneration(ctx))
            .Where(static m => m is not null)!;

        IncrementalValueProvider<(Compilation, ImmutableArray<RecordDeclarationSyntax>)> compilationAndRecords
            = context.CompilationProvider.Combine(recordDeclarations.Collect());

        context.RegisterSourceOutput(compilationAndRecords,
            static (spc, source) => Execute(source.Item1, source.Item2, spc));
    }

    private static bool IsSyntaxTargetForClassGeneration(SyntaxNode node) =>
        node is ClassDeclarationSyntax m && m.AttributeLists.Count > 0;

    private static bool IsSyntaxTargetForStructGeneration(SyntaxNode node) =>
        node is StructDeclarationSyntax m ? m.AttributeLists.Count > 0 : false;

    private static bool IsSyntaxTargetForRecordGeneration(SyntaxNode node) =>
       node is RecordDeclarationSyntax m ? m.AttributeLists.Count > 0 : false;

    private static ClassDeclarationSyntax? GetSemanticTargetForClassGeneration(GeneratorSyntaxContext context) {
        // we know the node is a ClassDeclarationSyntax thanks to IsSyntaxTargetForGeneration
        if (context.Node is ClassDeclarationSyntax classDeclarationSyntax) {
            // loop through all the attributes on the method
            foreach (AttributeListSyntax attributeListSyntax in classDeclarationSyntax.AttributeLists) {
                foreach (AttributeSyntax attributeSyntax in attributeListSyntax.Attributes) {
                    if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol) {
                        // weird, we couldn't get the symbol, ignore it
                        continue;
                    }

                    INamedTypeSymbol attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    string fullName = attributeContainingTypeSymbol.ToDisplayString();

                    // Is the attribute the [ComparisonOperators] attribute?
                    if (fullName == typeof(ComparisonOperatorsAttribute).FullName) {
                        // return the class
                        return classDeclarationSyntax;
                    }
                }
            }
        }

        // we didn't find the attribute we were looking for
        return null;
    }

    private static StructDeclarationSyntax? GetSemanticTargetForStructGeneration(GeneratorSyntaxContext context) {
        // we know the node is a ClassDeclarationSyntax thanks to IsSyntaxTargetForGeneration
        if (context.Node is StructDeclarationSyntax structDeclarationSyntax) {
            // loop through all the attributes on the method
            foreach (AttributeListSyntax attributeListSyntax in structDeclarationSyntax.AttributeLists) {
                foreach (AttributeSyntax attributeSyntax in attributeListSyntax.Attributes) {
                    if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol) {
                        // weird, we couldn't get the symbol, ignore it
                        continue;
                    }

                    INamedTypeSymbol attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    string fullName = attributeContainingTypeSymbol.ToDisplayString();

                    // Is the attribute the [ComparisonOperators] attribute?
                    if (fullName == typeof(ComparisonOperatorsAttribute).FullName) {
                        // return the class
                        return structDeclarationSyntax;
                    }
                }
            }
        }

        // we didn't find the attribute we were looking for
        return null;
    }

    private static RecordDeclarationSyntax? GetSemanticTargetForRecordGeneration(GeneratorSyntaxContext context) {
        // we know the node is a ClassDeclarationSyntax thanks to IsSyntaxTargetForGeneration
        if (context.Node is RecordDeclarationSyntax structDeclarationSyntax) {
            // loop through all the attributes on the method
            foreach (AttributeListSyntax attributeListSyntax in structDeclarationSyntax.AttributeLists) {
                foreach (AttributeSyntax attributeSyntax in attributeListSyntax.Attributes) {
                    if (context.SemanticModel.GetSymbolInfo(attributeSyntax).Symbol is not IMethodSymbol attributeSymbol) {
                        // weird, we couldn't get the symbol, ignore it
                        continue;
                    }

                    INamedTypeSymbol attributeContainingTypeSymbol = attributeSymbol.ContainingType;
                    string fullName = attributeContainingTypeSymbol.ToDisplayString();

                    // Is the attribute the [ComparisonOperators] attribute?
                    if (fullName == typeof(ComparisonOperatorsAttribute).FullName) {
                        // return the class
                        return structDeclarationSyntax;
                    }
                }
            }
        }

        // we didn't find the attribute we were looking for
        return null;
    }

    private static void Execute(Compilation compilation, ImmutableArray<ClassDeclarationSyntax> classes, SourceProductionContext context) {
        if (classes.IsDefaultOrEmpty) {
            // nothing to do yet
            return;
        }

        List<TypeToGenerate> classesToGenerate = GetClassToGenerate(compilation, classes.Distinct(), context.CancellationToken);
        if (classesToGenerate.Count == 0) {
            return;
        }

        foreach (TypeToGenerate classToGenerate in classesToGenerate) {
            string result = SourceGenerationHelper.ImplementComparisonOperators(classToGenerate);
            context.AddSource($"{classToGenerate.FullName}.g.cs", SourceText.From(result, Encoding.UTF8));
        }
    }

    private static void Execute(Compilation compilation, ImmutableArray<StructDeclarationSyntax> structs, SourceProductionContext context) {
        if (structs.IsDefaultOrEmpty) {
            // nothing to do yet
            return;
        }

        List<TypeToGenerate> classesToGenerate = GetStructToGenerate(compilation, structs.Distinct(), context.CancellationToken);
        if (classesToGenerate.Count == 0) {
            return;
        }

        foreach (TypeToGenerate classToGenerate in classesToGenerate) {
            string result = SourceGenerationHelper.ImplementComparisonOperators(classToGenerate);
            context.AddSource($"{classToGenerate.FullName}.g.cs", SourceText.From(result, Encoding.UTF8));
        }
    }

    private static void Execute(Compilation compilation, ImmutableArray<RecordDeclarationSyntax> records, SourceProductionContext context) {
        if (records.IsDefaultOrEmpty) {
            // nothing to do yet
            return;
        }

        List<TypeToGenerate> classesToGenerate = GetRecordToGenerate(compilation, records.Distinct(), context.CancellationToken);
        if (classesToGenerate.Count == 0) {
            return;
        }

        foreach (TypeToGenerate classToGenerate in classesToGenerate) {
            string result = SourceGenerationHelper.ImplementComparisonOperators(classToGenerate);
            context.AddSource($"{classToGenerate.FullName}.g.cs", SourceText.From(result, Encoding.UTF8));
        }
    }

    private static ComparisonOperatorsAttribute? GetComparisonOperatorsAttributeOrDefault(AttributeData? attributeData) {
        if (attributeData is null) {
            return null;
        }

        bool? implementIComparisonOperatorsInterface = attributeData.NamedArguments.FirstOrDefault(a => a.Key == nameof(ComparisonOperatorsAttribute.ImplementIComparisonOperatorsInterface)).Value.Value as bool?;

        ComparisonOperatorsAttribute attribute = new() { ImplementIComparisonOperatorsInterface = implementIComparisonOperatorsInterface.GetValueOrDefault() };

        return attribute;
    }

    private static List<TypeToGenerate> GetClassToGenerate(
        Compilation compilation,
        IEnumerable<ClassDeclarationSyntax> classes,
        CancellationToken ct) {

        List<TypeToGenerate> classesToGenerate = [];
        INamedTypeSymbol? comparisonOperatorsAttribute = compilation.GetTypeByMetadataName(typeof(ComparisonOperatorsAttribute).FullName);

        if (comparisonOperatorsAttribute is null) {
            // nothing to do if this type isn't available
            return classesToGenerate;
        }

        foreach (ClassDeclarationSyntax classDeclarationSyntax in classes) {
            // stop if we're asked to
            ct.ThrowIfCancellationRequested();

            SemanticModel semanticModel = compilation.GetSemanticModel(classDeclarationSyntax.SyntaxTree);
            if (semanticModel.GetDeclaredSymbol(classDeclarationSyntax) is not INamedTypeSymbol classSymbol) {
                // report diagnostic, something went wrong
                continue;
            }

            string name = classSymbol.Name;
            string nameSpace = classSymbol.ContainingNamespace.IsGlobalNamespace ? string.Empty : classSymbol.ContainingNamespace.ToString();
            string fullname = string.IsNullOrEmpty(nameSpace) ? name : $"{nameSpace}.{name}";

            string interfaceName = $"System.IComparable<{fullname}>";
            if (!classSymbol.AllInterfaces.Any(i => i.ToDisplayString().StartsWith(interfaceName))) {
                continue;
            }

            AttributeData? comparisonOperatorsAttibuteData = classSymbol.GetAttributes().FirstOrDefault(a => comparisonOperatorsAttribute?.Equals(a.AttributeClass, SymbolEqualityComparer.Default) ?? false);
            if (comparisonOperatorsAttibuteData is null) {
                continue;
            }

            var disposableValues = GetComparisonOperatorsAttributeOrDefault(comparisonOperatorsAttibuteData);

            classesToGenerate.Add(
                new TypeToGenerate(
                     name,
                     nameSpace,
                     (disposableValues?.ImplementIComparisonOperatorsInterface).GetValueOrDefault(),
                     false,
                     false
                )
            );
        }

        return classesToGenerate;
    }

    private static List<TypeToGenerate> GetStructToGenerate(
        Compilation compilation,
        IEnumerable<StructDeclarationSyntax> structs,
        CancellationToken ct) {

        List<TypeToGenerate> classesToGenerate = [];
        INamedTypeSymbol? comparisonOperatorsAttribute = compilation.GetTypeByMetadataName(typeof(ComparisonOperatorsAttribute).FullName);

        if (comparisonOperatorsAttribute is null) {
            // nothing to do if this type isn't available
            return classesToGenerate;
        }

        foreach (var structDeclarationSyntax in structs) {
            // stop if we're asked to
            ct.ThrowIfCancellationRequested();

            SemanticModel semanticModel = compilation.GetSemanticModel(structDeclarationSyntax.SyntaxTree);
            if (semanticModel.GetDeclaredSymbol(structDeclarationSyntax) is not INamedTypeSymbol classSymbol) {
                // report diagnostic, something went wrong
                continue;
            }

            string name = classSymbol.Name;
            string nameSpace = classSymbol.ContainingNamespace.IsGlobalNamespace ? string.Empty : classSymbol.ContainingNamespace.ToString();
            string fullname = string.IsNullOrEmpty(nameSpace) ? name : $"{nameSpace}.{name}";

            string interfaceName = $"System.IComparable<{fullname}>";
            if (!classSymbol.AllInterfaces.Any(i => i.ToDisplayString().StartsWith(interfaceName))) {
                continue;
            }

            AttributeData? comparisonOperatorsAttibuteData = classSymbol.GetAttributes().FirstOrDefault(a => comparisonOperatorsAttribute?.Equals(a.AttributeClass, SymbolEqualityComparer.Default) ?? false);
            if (comparisonOperatorsAttibuteData is null) {
                continue;
            }

            var disposableValues = GetComparisonOperatorsAttributeOrDefault(comparisonOperatorsAttibuteData);

            classesToGenerate.Add(
                new TypeToGenerate(
                     name,
                     nameSpace,
                     (disposableValues?.ImplementIComparisonOperatorsInterface).GetValueOrDefault(),
                     true,
                     false
                )
            );
        }

        return classesToGenerate;
    }

    private static List<TypeToGenerate> GetRecordToGenerate(
        Compilation compilation,
        IEnumerable<RecordDeclarationSyntax> records,
        CancellationToken ct) {

        List<TypeToGenerate> classesToGenerate = [];
        INamedTypeSymbol? comparisonOperatorsAttribute = compilation.GetTypeByMetadataName(typeof(ComparisonOperatorsAttribute).FullName);

        if (comparisonOperatorsAttribute is null) {
            // nothing to do if this type isn't available
            return classesToGenerate;
        }

        foreach (var recordDeclarationSyntax in records) {
            // stop if we're asked to
            ct.ThrowIfCancellationRequested();

            SemanticModel semanticModel = compilation.GetSemanticModel(recordDeclarationSyntax.SyntaxTree);
            if (semanticModel.GetDeclaredSymbol(recordDeclarationSyntax) is not INamedTypeSymbol classSymbol) {
                // report diagnostic, something went wrong
                continue;
            }

            string name = classSymbol.Name;
            string nameSpace = classSymbol.ContainingNamespace.IsGlobalNamespace ? string.Empty : classSymbol.ContainingNamespace.ToString();
            string fullname = string.IsNullOrEmpty(nameSpace) ? name : $"{nameSpace}.{name}";

            string interfaceName = $"System.IComparable<{fullname}>";
            if (!classSymbol.AllInterfaces.Any(i => i.ToDisplayString().StartsWith(interfaceName))) {
                continue;
            }

            AttributeData? comparisonOperatorsAttibuteData = classSymbol.GetAttributes().FirstOrDefault(a => comparisonOperatorsAttribute?.Equals(a.AttributeClass, SymbolEqualityComparer.Default) ?? false);
            if (comparisonOperatorsAttibuteData is null) {
                continue;
            }

            var disposableValues = GetComparisonOperatorsAttributeOrDefault(comparisonOperatorsAttibuteData);

            classesToGenerate.Add(
                new TypeToGenerate(
                     name,
                     nameSpace,
                     (disposableValues?.ImplementIComparisonOperatorsInterface).GetValueOrDefault(),
                     recordDeclarationSyntax.ClassOrStructKeyword.ToString() == "struct",
                     true
                )
            );
        }

        return classesToGenerate;
    }
}