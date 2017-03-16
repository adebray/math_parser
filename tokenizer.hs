-- A first attempt to use Parsec. This module will tokenize a string in inline math mode.

--import Control.Applicative ((<$>))
import Control.Monad (join)
import qualified Data.Map.Strict as Map -- better b/c this is small?
import Data.Tree
import Text.ParserCombinators.Parsec

type ParseTree = Tree String
type RuleMap = Map.Map String String
type ParseResult = Either String ParseTree
type ForestResult = Either String [ParseTree]

-- first: tokenizing. Removing spaces and converting TeX commands to single tokens.

-- letters contains everything that can follow a backslash in a command name
-- single_char_tokens contain all permissible ASCII single-character tokens.
letters            = ['A'..'Z'] ++ ['a'..'z']
non_letter_tokens  = "0123456789!^*()-_+=[]|':;<>,./?"
end_command        = " $\\" ++ non_letter_tokens
single_char_tokens = letters ++ non_letter_tokens

wrapped delims content = delims *> content <* delims

-- two helpers to generate a leaf and to tie together a group.
leaf_token :: String -> ParseTree
leaf_token str = Node str []

add_root :: [ParseTree] -> ParseTree
add_root = Node []

tex_command :: GenParser Char st String
tex_command = (:) <$> (char '\\') <*> (many $ noneOf end_command)

{- This function is probably the most confusing
    First, it throws away any spaces at the start or end,
    then it parses either a single token or a TeX command,
    and finally it makes that into a length-1 parse tree.
 -}
math_token :: GenParser Char st ParseTree
math_token =  wrapped spaces $ (fmap leaf_token) $ ((:[]) <$> oneOf single_char_tokens) <|> tex_command

math_group :: GenParser Char st ParseTree
math_group = between (char '{') (char '}') $ lifted_add_root (many token_or_group)
    where lifted_add_root = fmap add_root :: GenParser Char st [ParseTree] -> GenParser Char st ParseTree
-- issue: add_root expects a list of ParseTrees, and many token_or_group returns one object

token_or_group :: GenParser Char st ParseTree
token_or_group = math_group <|> math_token

-- accept string in $$, look for tokens
math_parser :: GenParser Char st ParseTree
math_parser = wrapped (char '$') $ add_root <$> many1 token_or_group
 
-- returns a list of tokens
-- I wish I didn't have to case on this
tokenize_math :: String -> Either String ParseTree
tokenize_math input = case parse math_parser "parse error :(" input of
    Left failure  -> Left $ show failure
    Right success -> Right success

-- now, rules.txt will contain all the rules for processing simple tokens (those without arguments)

-- add_tag div "Hello, world!" -> <div>Hello, world!</div>
add_tag :: String -> String -> String
add_tag tag text = concat ["<", tag, ">", text, "</", tag, ">"]

-- this will be (part of?) the function that reads a token and returns the HTML output.
-- the first argument is the map of substitutions that's assumed to already exist (I'll need to load
-- rules.txt, but that's later)
{-
group_to_HTML :: RuleMap -> ParseTree -> ParseTree -> String
group_to_HTML rules curr_node next_node
    | focus == ""  = next_children_parsed
    | focus == "_" = add_tag "sub" next_children_parsed
    | focus == "^" = add_tag "sup" next_children_parsed
    where focus = rootLabel node -- the next thing to process
          -- handle all the children of this node, then concatenate them and 
          -- TODO: in some cases, should advance; in others (''), should not advance.
          next_children_parsed = concatmap (group_to_HTML rules) (subforest next_node)
-}

-- issues: it's much easier to construct the parse tree so that _ and the group after it are
-- sibling nodes. But it's much harder to convert to HTML when that happens.

-- list of commands that take an argument, so I need to transform
arg_comms = ["^", "_", "\\mathbb", "\\mathbf", "\\mathcal", "\\mathscr", "\\mathrm", "\\mathtt",
             "\\mathit", "\\mathnormal", "\\mathfrak", "\\mathsf", "\\pmod", "\\text", "\\boldsymbol",
             "\\overline", "\\bar", "\\tilde", "\\hat"]
error_msg = "Parse Error: no argument to command "

-- possible solution: make another pass, where all operators in a list (_, ^, \mathbf, ...) are grafted
-- onto the next item in the list. either there's a group (in which case _ is just made into the rootLabel)
-- or there isn't (in which case a new tree is made, with one leaf
-- should be easy to do when separate.

-- I wonder if this pass can be made cleaner using Data.Traversable

-- given a tree, do what the above describes to its subForest. This doesn't change the root value, which is
-- useful for recursing.
-- Left indicates a parse error; Right indicates a successfully transformed tree
-- this looks very similar to graft_with_group, but they serve different roles. maybe I should combine them
graft_pass :: ParseTree -> ParseResult
graft_pass tree = Node (rootLabel tree) <$> traverse_forest (subForest tree)

-- handle all grafts in an array of trees. Cannot just call graft_pass on each one, because that calls
-- this function!
traverse_forest :: [ParseTree] -> ForestResult
traverse_forest [] = Right []
traverse_forest (focus:rest)
    | rootLabel focus `elem` arg_comms = make_graft focus rest
    | rest == []                       = Right (focus:[])
    | otherwise                        = (:) <$> graft_pass focus <*> traverse_forest rest

-- graft \mathbb {CP}
graft_with_group :: ParseTree -> ParseTree -> ParseResult
graft_with_group focus next = Node (rootLabel focus) <$> traverse_forest (subForest next)

-- graft \mathbb C
graft_two_leaves :: ParseTree -> ParseTree -> ParseTree
graft_two_leaves focus next = Node (rootLabel focus) $ (Node (rootLabel next) []):[]

-- makes a single graft. But has to address the rest of the array, too
make_graft :: ParseTree -> [ParseTree] -> ForestResult
make_graft focus [] = Left $ error_msg ++ rootLabel focus ++ "."
make_graft focus (next:after) = case rootLabel next of
    -- if the label is empty, it's a group, so we just merge the two trees
    "" -> (:) <$> (apply graft_with_group) <*> grafted_after
    -- if the label is nonempty, it's a single element, which becomes its own (single-leaf) tree
    _  -> ((apply graft_two_leaves):) <$> grafted_after
    where grafted_after = traverse_forest after
          apply fn = fn focus next

-- now, the next step is to parse the rules file, producing a Map...
-- and then apply the rules to everything in the tree.

-- aba "Side" "By" = "SideBySide"
aba :: String -> String -> String
aba s1 s2 = concat [s1, s2, s1]

-- remove all lines starting with # and empty lines
ignore_comments :: [String] -> [String]
ignore_comments = filter (\s -> not (null s) && s !! 0 /= '#')

-- produce an assignment of input |-> what to do for that input
single_rule :: [String] -> (String, String)
single_rule (input:output:"bin":[]) = (input, aba "&#x205F;" output)
single_rule (input:output:"op":[])  = (input, aba " " output) -- hopefully this is the right spacing
single_rule (input:output:[])       = (input, output)
single_rule _                       = ("", "")

generate_rules :: [String] -> RuleMap
generate_rules = Map.fromList . fmap (single_rule . words) . ignore_comments

-- apply_rule map str looks for str in the map, replaces it with its value if it's in the map,
-- and returns str if it's not in the map.
apply_rule :: RuleMap -> String -> String
apply_rule rules tok = case Map.lookup tok rules of
    Just value -> value
    Nothing    -> tok

-- for pretty-printing trees for testing, at least for now
-- eh make more explicit!
pretty_print_parse_tree :: String -> Either String String
pretty_print_parse_tree input = drawTree <$> join (graft_pass <$> tokenize_math input)

-- applies an argument to a group, flattening it
arg_to_group :: RuleMap -> ParseTree -> String
arg_to_group rules tree = case rootLabel tree of
   "_"        -> add_tag "sub" result
   "^"        -> add_tag "sup" result
   "\\mathit" -> add_tag "i" result
   '\\tilde'  -> result ++ "\x0303" -- if you call \tilde{abcde} this will look wrong.
   '\\hat'    -> result ++ "\x0302" -- same caveat applies to all combining marks
   '\\bar'    -> result ++ "\x0305" -- (also, will this cause issues with <i>h</i>bar not combining?)
   '\\vec'    -> result ++ "\x20D7"
   '\\check'  -> result ++ "\x030C"
   where result = codegen_forest rules tree

-- TODO codegen_forest, which actually traverses.
-- it does feel inefficient making yet another pass, but I can't see a better way
-- in particular, things like mathscr will use a modified version of the rules

-- traverses a 
codegen_forest :: RuleMap -> ParseTree -> String

-- finally, I'll try to simplify this code into using Foldable or Traversable



-- one final pass through the tree, to apply
-- will also have to account for _, ^, ...
code_generation :: RuleMap -> ParseTree -> String
code_generation rules tree = foldr (++) "" $ fmap (apply_rule rules) tree

readLines :: FilePath -> IO [String]
readLines = (fmap lines) . readFile 

tex_to_HTML :: RuleMap -> String -> String
tex_to_HTML rules input = case join (graft_pass <$> tokenize_math input) of
    Left fail -> fail
    Right succ -> code_generation rules succ

main = do input <- getLine
          rule_text <- readLines "rules.txt"
          let rules = generate_rules rule_text
          putStrLn (tex_to_HTML rules input)







-- main = do...


-- several tokens, including for now
-- ^ _ \mathbb \mathbf \mathcal \mathscr \mathrm \mathtt \mathit \mathnormal \mathfrak \mathsf \pmod \text
-- \boldsymbol \overline \bar \tilde \hat
-- accept one argument. I'll deal with them in the semantic (actual transformation) stage


-- Probably not handling right now:
-- \left \right (and all their variants, e.g. \bigl \bigr)
-- \sqrt
-- \frac \binom \tfrac \dfrac etc. \smallmatrix and variants
-- \grave \dot \ddot \acute (etc.)
-- \not (don't know how, which is unfortunate)
