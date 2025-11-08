use crate::parser::abstract_expression::AbstractExpression;

const STYLE: &str = r#"<style>

*{
    width: fit-content;
    margin-inline: auto;
    color: white;
    background-color: black;
}

div {
    border: 1px solid;
    padding: 5px;
}

td>div {
    border: none;
}

td {
    border: 1px solid;
}

hr {
    width: 100%;
}

</style>"#;

fn as_html_string(abstract_expression: &AbstractExpression) -> String {
    let mut out = String::from("<div>");

    match abstract_expression {
        AbstractExpression::IntLiteral(token)
        | AbstractExpression::RealLiteral(token)
        | AbstractExpression::Undefined(token)
        | AbstractExpression::BoolLiteral(token) => out += &token.value,
        AbstractExpression::UnaryOperation(token, ae) => {
            out += &(token.value.clone() + &as_html_string(ae))
        }
        AbstractExpression::BinaryOperation(ae1, token, ae2) => {
            out += &(as_html_string(ae1) + &token.value + &as_html_string(ae2))
        }
        AbstractExpression::Block(abstract_block) => out += "BLOCK",
        AbstractExpression::Map(abstract_map) => {
            let mut lst = String::from("<table>");

            for i in &abstract_map.mappings {
                lst += &("<tr>".to_owned()
                    + "<td>"
                    + &as_html_string(&i.0)
                    + "</td><td>"
                    + &as_html_string(&i.1)
                    + "</td></tr>")
            }

            lst += "</table>";
            out += &lst;
        }
        AbstractExpression::Set(abstract_set) => {
            let mut lst = String::from("<ul>");

            for i in &abstract_set.items {
                lst += &("<li>".to_owned() + &as_html_string(i) + "</li>")
            }

            lst += "</ul>";
            out += &lst;
        }
        AbstractExpression::List(abstract_list) => {
            let mut lst = String::from("<ol>");

            for i in &abstract_list.items {
                lst += &("<li>".to_owned() + &as_html_string(i) + "</li>")
            }

            lst += "</ol>";
            out += &lst;
        }
        AbstractExpression::Capture(abstract_capture) => out += "CAPTURE",
        AbstractExpression::Call(abstract_call) => out += "CALL",
        AbstractExpression::EmptySet => out += "{}",
        AbstractExpression::VariableReference(var) => {
            out += &format!(
                "{:?}",
                var.identities
                    .iter()
                    .map(|t| &t.value)
                    .into_iter()
                    .collect::<Vec<_>>()
            )
        }
        AbstractExpression::StrLiteral(token) => out += &token.value,
        AbstractExpression::Raise(abstract_expression) => {
            out += "RAISE";
            out += &as_html_string(abstract_expression);
        }
        AbstractExpression::Group(abstract_expression) => {
            out += "GROUP";
            out += &as_html_string(abstract_expression)
        }
        AbstractExpression::Negate(abstract_expression) => {
            out += "(-1) * ";
            out += &as_html_string(abstract_expression)
        }
    };

    out += "</div>";

    return out;
}

pub(super) fn send_html(abstract_expression: &AbstractExpression) {
    let html = as_html_string(abstract_expression);
    let s = std::fs::read_to_string("parser.html")
        .or(Ok::<_, ()>(String::new()))
        .unwrap();
    std::fs::write("parser.html", s + "<hr>" + &html + STYLE).unwrap();
    // std::io::stdin().read_line(&mut String::new()).unwrap();
}
