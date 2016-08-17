  Conditional guards -> do
    final <- newLabel
    abort <- newLabel
    genGuards guards abort final

    setLabel abort $ branch final
    createTagIf final (from loc)
    return ()

genGuards :: [AST] -> Name -> Name -> LLVM ()
genGuards [guard] none one = genGuard guard none

genGuards (guard:xs) none one = do
  next <- newLabel
  genGuard guard next
  setLabel next $ branch one
  genGuards xs none one

genGuard :: AST -> Name -> LLVM ()
genGuard (AST _ _ _ (Guard guard acc)) next = do
  tag  <- createExpression guard
  code <- newLabel
  setLabel code $ condBranch tag code next
  createInstruction acc

setLabel :: Name -> Named Terminator -> LLVM()
setLabel name t800 = do
    addBasicBlock t800
    blockName .= name

addBasicBlock :: Named Terminator -> LLVM ()
addBasicBlock t800 = do
    lins  <- use instrs
    bbl   <- use bblocs
    name  <- use blockName
    name' <- newLabel
    blockName .= name'
    instrs    .= Seq.empty
    bblocs    .= bbl Seq.|> BasicBlock name (toList lins) t800