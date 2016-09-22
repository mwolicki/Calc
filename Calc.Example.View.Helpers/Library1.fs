namespace Calc.Example.View.Helpers

open System.Windows.Documents
module Helpers =
    open Analyse
    open Tokenizer
    open System.Windows.Media

    let applyFormatting (expr:Expr) (textBox : System.Windows.Controls.RichTextBox) =
        let getStartAndEnd (t:Token) =
            let (startPos, endPos) = t.Pos
            let start = textBox.Document.ContentStart
            start.GetPositionAtOffset (int startPos + 2, LogicalDirection.Forward), start.GetPositionAtOffset (int endPos + 3, LogicalDirection.Forward)
        let rec apply' = function
            | ConstStr (_, [t])
            | ConstNum (_, [t])
            | ConstBool (_, [t])
            | ConstDateTime (_, [t])
            | ConstDate (_, [t]) -> 
                let (startPtr, endPtr) =getStartAndEnd t
                let textRange = TextRange(startPtr, endPtr)
                textRange.ApplyPropertyValue(TextElement.BackgroundProperty, SolidColorBrush(Colors.Red))

            | Reference (_, ts) -> ()
            | OperatorCall (_,lhs,rhs, ts) ->
                apply' lhs
                apply' rhs
            | Negate (inner, ts)
            | Group (inner, ts) -> apply' inner
            | FunctionCall (_, inner, ts) ->
                List.iter apply' inner
            | _ -> ()
        let textRange = TextRange(textBox.Document.ContentStart, textBox.Document.ContentEnd)
        textRange.ClearAllProperties()
        apply' expr
