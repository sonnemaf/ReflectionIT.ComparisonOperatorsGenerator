using System.Collections.Immutable;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

namespace ReflectionIT.ComparisonOperatorsGenerator;

[Generator]
public class SourceGenerator : IIncrementalGenerator {

    public const string FullyQualifiedAttributeName = "ReflectionIT.ComparisonOperatorsGenerator.Attributes.ComparisonOperatorsAttribute";

    public void Initialize(IncrementalGeneratorInitializationContext context) {

        context.RegisterPostInitializationOutput(ctx => ctx.AddSource(
            $"{FullyQualifiedAttributeName}.g.cs", SourceText.From(SourceGenerationHelper.Attribute, Encoding.UTF8)));

        var classDeclarations = context.SyntaxProvider.ForAttributeWithMetadataName(
           FullyQualifiedAttributeName,
           predicate: (node, cancel) =>
               node is RecordDeclarationSyntax or ClassDeclarationSyntax or StructDeclarationSyntax,
           transform: (context, cancel) =>
               context.SemanticModel.GetDeclaredSymbol(context.TargetNode, cancel) as ITypeSymbol
       )
       .Where(typeSymbol => typeSymbol is not null)
       .Select((typeSymbol, cancel) => typeSymbol!);

        var collectedTypeSymbols = classDeclarations.Collect();

        context.RegisterSourceOutput(collectedTypeSymbols, GenerateSource);
    }

    private void GenerateSource(SourceProductionContext context, ImmutableArray<ITypeSymbol> typeSymbols) {
        if (typeSymbols.IsDefaultOrEmpty) {
            return;
        }

        foreach (var typeSymbol in typeSymbols) {
            var filename = typeSymbol.ToDisplayString()
                              .Replace('<', '{')
                              .Replace('>', '}')
                              .Replace(" ", string.Empty);

            string result = SourceGenerationHelper.ImplementComparisonOperators(typeSymbol);

            context.AddSource($"{filename}.g.cs", SourceText.From(result, Encoding.UTF8));
        }
    }
}