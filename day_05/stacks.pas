program SupplyStacks;
{ Solution for Day 5: Supply Stacks}

type { Type declarations }

    { Pointer to CrateNode}
    CrateNodePtr = ^CrateNode;
    { Node of a linked list containing the crates of one stack }
    CrateNode = record
        value: char;
        next: CrateNodePtr;
    end;

    { Pointer to StackNode}
    StackNodePtr = ^StackNode;
    { Node of the linked list containing stacks }
    StackNode = record
        top_crate: CrateNodePtr;
        bottom_crate: CrateNodePtr;
        next: StackNodePtr;
    end;

var { Global variables }

    stacks1, stacks2: StackNodePtr;

{ Functions }

(* Returns a pointer to the stack at position i.
   The index i starts at 0. *)
function getStackAtIndex(i: byte; stacks: StackNodePtr): StackNodePtr;
begin
    if stacks = nil then
    begin
        writeln('Error: Index out of bounds.');
        halt(1);
    end;
    if i = 0 then
        getStackAtIndex := stacks
    else
        getStackAtIndex := getStackAtIndex(i - 1, stacks^.next);
end;

{ Procedures }

{ Prints crates for debug purposes }
procedure printCrates(crates: CrateNodePtr);
begin
    if crates <> nil then
    begin
        write(crates^.value, ' ');
        printCrates(crates^.next);
    end
    else writeln('');
end;

{ Prints stacks for debug purposes }
procedure printStacks(stacks: StackNodePtr);
begin
    if stacks <> nil then
    begin
        write('-> ');
        printCrates(stacks^.top_crate);
        printStacks(stacks^.next);
    end;
end;

{ Prints the values of the top crates }
procedure printTopCrates(stacks: StackNodePtr);
begin
    if stacks <> nil then
    begin
        if stacks^.top_crate <> nil then write(stacks^.top_crate^.value);
        printTopCrates(stacks^.next);
    end;
end;

{ Adds a crate to the bottom of the stack }
procedure addCrateToBottom(c: char; var stack: StackNode);
begin
    if stack.top_crate = nil then
    begin { stack is empty }
        new(stack.top_crate);
        stack.bottom_crate := stack.top_crate;
    end
    else
    begin { stack is not empty }
        new(stack.bottom_crate^.next);
        stack.bottom_crate := stack.bottom_crate^.next;
    end;
    stack.bottom_crate^.value := c;
    stack.bottom_crate^.next := nil;
end;

(* Processes a line from the input file that contains
   the initial arrangement of crates *)
procedure processCratesLine(var line: string; var stacks: StackNodePtr);
var
    i: integer;
    line_length: byte;
    curr_stack: ^StackNodePtr;
begin
    i := 2;
    line_length := ord(line[0]);
    curr_stack := @stacks;

    while i <= line_length do
    begin
        if curr_stack^ = nil then
        begin { Create a new stack }
            new(curr_stack^);
            curr_stack^^.top_crate := nil;
            curr_stack^^.bottom_crate := nil;
            curr_stack^^.next := nil;
        end;
        if line[i] <> ' ' then
            addCrateToBottom(line[i], curr_stack^^);
        i := i + 4;
        curr_stack := @curr_stack^^.next;
    end;
end;

(* Moves one crate from the source stack to the destination stack.
   The bottom_crate pointers are not maintained any more. *)
procedure moveCrate(var source, destination: StackNode);
var
    dest_top: CrateNodePtr;
begin
    if source.top_crate = nil then
    begin
        writeln('Error: Source stack is empty.');
        halt(1);
    end;
    { The move is done by manipulating pointers }
    dest_top := destination.top_crate;
    destination.top_crate := source.top_crate;
    source.top_crate := source.top_crate^.next;
    destination.top_crate^.next := dest_top;
end;

(* Moves item number of crates from the source stack
   to the destination stack one-by-one *)
procedure moveCratesOneByOne(items: integer; source, destination: StackNodePtr);
var
    i: integer;
begin
    for i := 1 to items do
        moveCrate(source^, destination^);
end;

(* Moves item number of crates from the source stack
   to the destination stack maintaining the order *)
procedure moveCratesAtOnce(items: integer; source, destination: StackNodePtr);
var
    temp: StackNode;
begin
    temp.top_crate := nil;
    temp.bottom_crate := nil;
    temp.next := nil;
    moveCratesOneByOne(items, source, @temp);
    moveCratesOneByOne(items, @temp, destination);
end;

(* Processes a line from the input file that contains
   the instructions on how to move crates *)
procedure processMoveLine(var line: string; stacks1, stacks2: StackNodePtr);
var
    ord_one, f_index, source_index, dest_index: byte;
    items: integer;
    error_code: word;
begin
    ord_one := ord('1');
    f_index := pos('f', line);
    { Number of items to move }
    val(copy(line, 6, f_index - 7), items, error_code);
    if error_code <> 0 then
    begin
        writeln('Error parsing the number of items.');
        halt(1);
    end;
    { Index of the source crate }
    source_index := ord(line[f_index + 5]) - ord_one;
    { Index of the destination crate }
    dest_index := ord(line[f_index + 10]) - ord_one;

    moveCratesOneByOne(items, getStackAtIndex(source_index, stacks1),
        getStackAtIndex(dest_index, stacks1));
    moveCratesAtOnce(items, getStackAtIndex(source_index, stacks2),
        getStackAtIndex(dest_index, stacks2));
end;

{ Reads and processes the input file }
procedure processFile(input_file_name: string; var stacks1, stacks2: StackNodePtr);
var
    input_file: text;
    line: string;
begin
    { Opening the input file }
    {$I-}
    assign(input_file, input_file_name);
    reset(input_file);
    {$I+}
    if IoResult <> 0 then
    begin
        writeln('Error opening file ', input_file_name, '.');
        halt(1);
    end;
    { Processing the input file }
    while not eof(input_file) do
        begin
            readln(input_file, line);
            if ord(line[0]) >= 3 then
                if line[2] = 'o' then processMoveLine(line, stacks1, stacks2)
                else if '1' <> line[2] then
                begin
                    processCratesLine(line, stacks1);
                    processCratesLine(line, stacks2);
                end;
        end;
    close(input_file);
end;

{ Frees up the linked list of crates for good housekeeping }
procedure destroyCrates(var crates: CrateNodePtr);
begin
    if crates <> nil then
    begin
        destroyCrates(crates^.next);
        dispose(crates);
        crates := nil;
    end
end;

{ Frees up the linked list of stacks for good housekeeping }
procedure destroyStacks(var stacks: StackNodePtr);
begin
    if stacks <> nil then
    begin
        destroyCrates(stacks^.top_crate);
        destroyStacks(stacks^.next);
        dispose(stacks);
        stacks := nil;
    end;
end;

begin { The program starts here }
    if paramCount <> 1 then
    begin
        writeln('Error: The program must be started with the input file being the only parameter.');
        halt(1);
    end;
    if paramStr(1) = '' then
    begin
        writeln('Error: The input file name is empty.');
        halt(1);
    end;

    writeln('Program started.');
    stacks1 := nil;
    stacks2 := nil;
    processFile(paramStr(1), stacks1, stacks2);
(*
    writeln('Final configuration of the stacks for part 1:');
    printStacks(stacks1);
    writeln('Final configuration of the stacks for part 2:');
    printStacks(stacks2);
*)
    write('Crates that end up on top of each stack in part 1: ');
    printTopCrates(stacks1);
    writeln('');
    write('Crates that end up on top of each stack in part 2: ');
    printTopCrates(stacks2);
    writeln('');

    destroyStacks(stacks1);
    destroyStacks(stacks2);
end.
