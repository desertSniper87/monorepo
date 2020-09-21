unit Hash_;

interface

uses
  SysUtils,
  Windows,
  Classes,
  SokUtil_;

////////////////////////////////////////////////////////////////////////////////
// Hash-tables
////////////////////////////////////////////////////////////////////////////////

type
  // hash-table items, with:
  // * hash-values for speeding up the search;
  // * double-links for fast deletion;
  THashTableItem=class
  protected
    HashKey:THashTableHashKey;
    Next:THashTableItem;
    Prev:THashTableItem;
  public
    constructor Create(HashKey__:THashTableHashKey);

    function    ToString:String; reintroduce; virtual;
  end;

  // hash-table items where keys are strings;
  // note that this is a general class without any values attached to the key;
  // to create key/value pairs, define a derived class including a value-field;
  TStringKeyHashTableItem=class(THashTableItem)
  private
    fKey:String;
  public
    constructor Create(const Key__:String);
    destructor  Destroy; override;
    function    ToString:String; override;
    property    Key:String read fKey;
  end;

  // hash-table items where keys and values are strings
  TStringKeyAndValueHashTableItem=class(TStringKeyHashTableItem)
  public
    Value:String;
    constructor Create(const Key__,Value__:String);
    destructor  Destroy; override;
    function    ToString:String; override;
  end;

  THashTableForEachFunction = function(HashTableItem__:THashTableItem):Boolean; // if the function returns 'False', the iteration stops; otherwise the iteration continues to the next item

  THashTableItemVector=array[0..(MaxInt div SizeOf(THashTableItem))-1] of THashTableItem;
  PHashTableItemVector=^THashTableItemVector;

  // a hash table implementation using Larson's algorithm which excels in fast
  // dynamic resizing of the table because resizing doesn't require all items to
  // be re-hashed; the table grows/shrinks with one bucket at a time, and only
  // items in one bucket require re-hashing when that happens
  THashTable=class
  private
    ActivatedBucketCount:Integer; // the buckets grows/shrinks with one bucket at a time
    AllocatedBucketCount:Integer; // extra buckets above 'ActivatedBucketCount' are allocated during expansion in the hope that it reduces the number of bucket reallocations
    Buckets:PHashTableItemVector;
    fCount:Integer; // number of items in the table
    HashBitMask:Integer; // a (2^n - 1) number where n is an integer >= 0
    // 'LoadFactor' is the targeted bucket-size, i.e., the average number of
    // collisions during lookup;
    // the capacity is the number of buckets multiplied by the loadfactor,
    // so for a fast calculation of the capacity using a bit-shift, the
    // loadfactor is rounded to a 2^n number;
    // doing it this way avoids using space for storing a 'Capacity' in the table;
    LoadFactorPowerOf2Exponent:Integer; // may be defined as 'Byte' if the saved bytes can be used for something better
  protected
    function    BucketIndex(Key__:THashTableHashKey):Integer; // maps hash-keys to bucket numbers
    function    BucketSize(Index__:Integer):Integer; // returns the number of items in a bucket
    function    GetCapacity:Integer;
    function    GetLoadFactor:Integer;
    function    Reallocate(Capacity__:Integer):Boolean;
    procedure   SetCapacity(Capacity__:Integer);
    procedure   SetLoadFactor(LoadFactor__:Integer);
    function    ShowBucket(Index__:Integer):Integer;
  public
    constructor Create(CapacityHint__,LoadFactor__:Integer); virtual;
    destructor  Destroy; override;

    procedure   Add(Item__:THashTableItem); virtual;
    procedure   Clear; virtual;
    procedure   Delete(var Item__:THashTableItem); virtual;
    function    ForEach(ForEachFunction__:THashTableForEachFunction):Integer;
    function    Lookup(HashKey__:THashTableHashKey; var Item__:THashTableItem):Boolean;
    function    Stats:Integer;

    property    Capacity:Integer read GetCapacity write SetCapacity; // number of buckets * loadfactor
    property    Count:Integer read fCount; // number of items in the table
    property    LoadFactor:Integer read GetLoadFactor write SetLoadFactor; // targeted number of items per bucket
  end;

  // hash-table with items where keys are strings;
  // the items stored in the table must descend from 'TStringKeyHashTableItem'
  TStringKeyHashTable=class(THashTable)
  public
    function    Lookup(const Key__:String; var Item__:TStringKeyHashTableItem):Boolean;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// Utilities
////////////////////////////////////////////////////////////////////////////////


function Log2(Number__:Integer):Integer; forward;
function IsAPowerOf2(Number__:Integer):Boolean; forward;

function CeilingLog2(Number__:Integer):Integer; // Log2() rounded upwards;
begin // precondition: 'Number__' >= 0
  Result:=Succ(Log2(Pred(Number__)));
end;

function IsAPowerOf2(Number__:Integer):Boolean;
begin // precondition: 'Number__' >= 0
  Result:=(Number__ and (Number__-1))=0;
end;

function Log2(Number__:Integer):Integer;
begin
  Result:=-1;
  while Number__>0 do begin Inc(Result); Number__:=Number__ div 2; end;
end;

function MultiplicationOverflow(Multiplicand__,Multiplier__,HighValue__:Integer):Boolean;
begin
  Result:=HighValue__ div Multiplier__ < Multiplicand__;
end;

function PowerOf2(Number__:Integer):Integer; // precondition: 0 <= 'Number__' <= number of bits in highest integer value - 1
begin
  Result:=1 shl Number__;
end;

{
function  StrHashValuePJW(const Text__:String):THashTableHashKey; // a hashpjw-based function (Peter J. Weinberger), adding in the length of the text, and with an upper limit on the number of hashed characters
const BITS_PER_BYTE         = Hash_.BITS_PER_BYTE;
      BITS_PER_HASH_KEY     = SizeOf(THashTableHashKey) * BITS_PER_BYTE;
      HIGH_4_BITS           = THashTableHashKey($f) shl (BITS_PER_HASH_KEY-4);
      MAX_TESTED_CHARACTERS = 64;
var   i,Count,Index,Step:Integer; HighBits:THashTableHashKey;
begin
  Result:=Length(Text__);
  if Result<=MAX_TESTED_CHARACTERS then begin
     Count:=Result; Step:=1; // test the complete text
     end
  else begin // test characters distributed over the entire text
     Count:=MAX_TESTED_CHARACTERS; Step:=Result div MAX_TESTED_CHARACTERS;
     end;
  Index:=1;
  for i:=1 to Count do begin
      Result:=(Result shl 4) + Ord(Text__[Index]);
      HighBits:=Result and HIGH_4_BITS;
      if HighBits<>0 then Result:=(Result xor (HighBits shr (BITS_PER_HASH_KEY-8))) xor HighBits; // the high-bits are cleared after xor'ing them with the bits 4..7
      Inc(Index,Step); // 'Index' is the next character number to test
      end;
end;
}
{
function  StrHashValueDEK(const Text__:String):THashTableHashKey; // a hashdek function (Donald E. Knuth, "The Art Of Computer Programming Volume 3", 6.4)
var Index:Integer;
begin
  Result:=Length(Text__);
  for Index:=1 to Result do
      Result:=((Result shr 5) xor (Result shl 27)) xor Ord(Text__[Index]);
end;
}

{
function  StrHashValue(const Text__:String):THashTableHashKey;
// according to Aho, taking the sum of the text interpreted as numbers is almost
// as good a hash-function as the hashpjw function, and summing is probably faster
// (provided the text is machine-word aligned);
// however, tests have not been able to confirm Aho's good results for sums,
// so it's not in production
const MAX_TESTED_MACHINE_WORDS = 64;
      UNALIGNED_BIT_MASK       = SizeOf(Cardinal)-1; // precondition: SizeOf(Cardinal) is a 2^n number where n is an integer > 0, but there are no known processors where this doesn't hold
var   Index,Count,Step:Cardinal; pNumber:^Cardinal;
begin
  Result:=Length(Text__);
  Count:=Result div SizeOf(Cardinal); // the truncated number of machine-words in the text

  // first sum the last 0..SizeOf(Cardinal)-1 characters in the text (assuming
  // that the text is machine-word aligned);
  // it's practical to do this first while the length of the text still is
  // available in 'Result'
  for Index:=Result downto Succ(Count*SizeOf(Cardinal)) do
      Inc(Result,Ord(Text__[Index]));

  if  Count>0 then begin
      pNumber:=Addr(Text__[1]);
      if   (Cardinal(pNumber) and UNALIGNED_BIT_MASK)<>0 then begin
           // in Delphi, the text is always machine-word aligned,
           // but for completeness, the test is here anyway
           for Index:=1 to UNALIGNED_BIT_MASK do Inc(Result,Ord(Text__[Index])); // sum the leading characters before the alignment
           Dec(Count); pNumber:=Pointer(Cardinal(pNumber) and (not UNALIGNED_BIT_MASK)); // align to next machine-word boundary;
           end;

      if   Count<=MAX_TESTED_MACHINE_WORDS then
           Step:=1 // test the complete text
      else begin
           Step:=Count div MAX_TESTED_MACHINE_WORDS; // test machine-words distributed over the entire text
           Count:=MAX_TESTED_MACHINE_WORDS;
           end;

       for Index:=1 to Count do begin // iterate over the text and sum the machine-words
           //Inc(Result,pNumber^);
           Result:=(Result shl 4) + pNumber^;
           //Result:=Result xor pNumber^;
           Inc(pNumber,Step);
           end;
     end;
end;
}

////////////////////////////////////////////////////////////////////////////////
// Hash-tables
////////////////////////////////////////////////////////////////////////////////

const
  DEFAULT_HASH_TABLE_LOAD_FACTOR = 8;
  HASH_TABLE_SHRINK_BUCKET_COUNT_THRESHOLD = 2; // allow this number of extra buckets before shrinking the table

var
  HashTableResizeCount        : Integer = 0; // for internal statistics
  MAX_HASH_TABLE_BUCKET_COUNT : Integer = 0; // calculated by 'InitializeHashTables' and constant thereafter

procedure InitializeHashTables;
begin
  // the maximum number of buckets is a 2^n number where 'n' is an integer >= 0
  MAX_HASH_TABLE_BUCKET_COUNT:=PowerOf2(Log2(Succ(High(THashTableItemVector)-Low(THashTableItemVector)))); // 'Succ(High()-Low())': this is the maximum number of buckets in the bucket-vector type
end;

function HashTableItemShow(HashTableItem__:THashTableItem):Boolean;
begin
  Result:=True;
  with HashTableItem__ do Write(ToString,SPACE);
end;

////////////////////////////////////////////////////////////////////////////////
// THashTableItem - hash table items
////////////////////////////////////////////////////////////////////////////////

constructor THashTableItem.Create(HashKey__:THashTableHashKey);
begin
  HashKey:=HashKey__; Next:=nil; Prev:=nil;
end;

function    THashTableItem.ToString:String;
begin
  Result:=IntToStr(HashKey);
end;

////////////////////////////////////////////////////////////////////////////////
// TStringKeyHashTableItem - hash table items where keys are strings
////////////////////////////////////////////////////////////////////////////////

constructor TStringKeyHashTableItem.Create(const Key__:String);
begin
  Inherited Create(StrHashValuePJW(Key__));
  fKey:=Key__;
end;

destructor  TStringKeyHashTableItem.Destroy;
begin
  fKey:='';
  Inherited;
end;

function    TStringKeyHashTableItem.ToString:String;
begin
  Result:=Key;
end;

////////////////////////////////////////////////////////////////////////////////
// TStringKeyAndValueHashTableItem - hash-table items with key/value strings
////////////////////////////////////////////////////////////////////////////////

constructor TStringKeyAndValueHashTableItem.Create(const Key__,Value__:String);
begin
  Inherited Create(Key__);
  Value:=Value__;
end;

destructor  TStringKeyAndValueHashTableItem.Destroy;
begin
  Value:='';
  Inherited;
end;

function    TStringKeyAndValueHashTableItem.ToString:String;
begin
  Result:=IntToStr(HashKey)+COLON+SPACE+Key+SPACE+COLON+SPACE+Value;
end;

////////////////////////////////////////////////////////////////////////////////
// THashTable - hash tables
////////////////////////////////////////////////////////////////////////////////

constructor THashTable.Create(CapacityHint__,LoadFactor__:Integer);
begin
  AllocatedBucketCount:=0; ActivatedBucketCount:=0;
  HashBitMask:=0; Buckets:=nil; fCount:=0;
  if   LoadFactor__>0 then SetLoadFactor(LoadFactor__)
  else SetLoadFactor(DEFAULT_HASH_TABLE_LOAD_FACTOR);
  if   not Reallocate(CapacityHint__) then
       raise Exception.Create('THashTable.Create: '+TEXT_TASK_FAILED);
end;

destructor  THashTable.Destroy;
begin
  Clear;
end;

procedure   THashTable.Add(Item__:THashTableItem);
var Index:Integer; First,This:THashTableItem;

  function ExpandTable:Boolean;
  var BuddyBucketIndex,NewBucketIndex,NewBucketCount:Integer;
      First,Item,Next:THashTableItem;
  begin
    Result:=True;

    if ActivatedBucketCount>=AllocatedBucketCount then begin // 'True': allocate more buckets; 25% more buckets are added
       NewBucketCount:=AllocatedBucketCount+Min(Max(1,AllocatedBucketCount div 4),MAX_HASH_TABLE_BUCKET_COUNT-AllocatedBucketCount);
       if   SafeRealloc(Pointer(Buckets),SizeOf(Buckets^[Low(Buckets^)])*AllocatedBucketCount,SizeOf(Buckets^[Low(Buckets^)])*NewBucketCount,False,False) then begin
            //Writeln('Expand ',AllocatedBucketCount,' => ',NewBucketCount);
            AllocatedBucketCount:=NewBucketCount;
            Inc(HashTableResizeCount);
            end
       else Result:=False;
       end;

    if ActivatedBucketCount<AllocatedBucketCount then begin // 'True': more buckets are available
       NewBucketIndex:=ActivatedBucketCount; // 'NewBucketIndex' is 0-based, hence, this sets it to the next, not yet used bucket
       BuddyBucketIndex:=NewBucketIndex mod Max(1,(Succ(HashBitMask) div 2));
       Buckets[NewBucketIndex]:=nil; // initialize the new bucket

       Inc(ActivatedBucketCount);
       if ActivatedBucketCount>Succ(HashBitMask) then begin // 'True': add the next power of 2 to the hash-bitmask
          HashBitMask:=Max(1,Succ(HashBitMask*2));
          end;

       //if ActivatedBucketCount<=50 then Writeln(NewBucketIndex,SPACE,BuddyBucketIndex,' :: ',Succ(HashBitMask));

       // split the 'buddy' bucket and move the items that now hash to the new bucket
       if BuddyBucketIndex>=0 then begin
          First:=Buckets[BuddyBucketIndex]; Item:=First;
          if Assigned(Item) then // 'True': the 'buddy' bucket isn't empty
             repeat if First=nil then First:=Item; // 'True': the 'buddy' bucket lost its root in the prior loop
                    Next:=Item.Next; // remember the next item; otherwise, this information isn't available at the end of the loop if the item moves to the new bucket
                    Index:=BucketIndex(Item.HashKey);
                    if   Index=BuddyBucketIndex then begin
                         // the item still belongs to the 'buddy' bucket, hence, do nothing
                         end
                    else if Index=NewBucketIndex then with Item do begin
                            if Item=First then begin // the 'buddy' bucket looses its root item; update it
                               if   First<>Next then begin // 'True': 'First' isn't the only item in the 'buddy' bucket
                                    First:=nil; // trick the 'repeat' ... 'until' loop so it continues to the next item
                                    Buckets[BuddyBucketIndex]:=Next;
                                    end
                               else Buckets[BuddyBucketIndex]:=nil; // the 'buddy' bucket looses its last item, hence, clear it
                               end;

                            Prev.Next:=Next; Next.Prev:=Prev; // remove 'Item' from the chain in the 'Buddy' bucket
                            if Buckets[NewBucketIndex]<>nil then begin // 'True': the new bucket isn't empty, hence, insert 'Item' as the last member of the chain in the new bucket
                               Item.Next:=Buckets[NewBucketIndex];
                               Item.Prev:=Item.Next.Prev;
                               Item.Next.Prev:=Item; // update neighboring items in the chain
                               Item.Prev.Next:=Item; // update neighboring items in the chain
                               end
                            else begin // the new bucket is empty; insert 'Item' as its first item
                               Buckets[NewBucketIndex]:=Item;
                               Item.Prev:=Item; Item.Next:=Item; // make a circular list with this single item
                               end;
                            end
                         else
                            raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['Hash-table indexing corrupted.']));
                    Item:=Next;
             until  Item=First;
          end;
       end;
  end;

begin // Add
  if Count>=ActivatedBucketCount shl LoadFactorPowerOf2Exponent then ExpandTable; // expand the table when 'Count' >= 'Capacity'
  if Count<High(Count) then begin
     Index:=BucketIndex(Item__.HashKey);
     First:=Buckets[Index]; // get the first item in this bucket
     if Assigned(First) then with Item__ do begin
        This:=First;
        if HashKey<This.Prev.HashKey then begin // 'True': 'Item__' isn't a new last item in this bucket
           while HashKey>=This.HashKey do This:=This.Next; // sort the items in ascending order on hash-key
           if This=First then Buckets[Index]:=Item__; // 'Item__' is a new first item in the bucket
           end;
        Prev:=This.Prev; Next:=This; // insert the new item at its proper position in the chain
        Prev.Next:=Item__; Next.Prev:=Item__; // update neighboring items in the chain
        end
     else with Item__ do begin // this is an empty bucket; make a new circular list with 'Item__' as the only member
        Next:=Item__; Prev:=Item__; Buckets[Index]:=Item__;
        end;
     Inc(fCount);
     end
  else
     raise Exception.Create(TEXT_MEMORY_FULL);
end;

function    THashTable.BucketIndex(Key__:THashTableHashKey):Integer;
begin
  Result:=Key__ and HashBitMask;
  if Result>=ActivatedBucketCount then // 'True': the bucket isn't in use yet, hence, use it's 'buddy-bucket' instead
     Result:=Result and (HashBitMask div 2);
end;

function    THashTable.BucketSize(Index__:Integer):Integer;
var First,Item:THashTableItem;
begin
  Result:=0;
  if (Index__>=Low(Buckets^)) and (Index__<ActivatedBucketCount) then begin
     First:=Buckets^[Index__]; Item:=First;
     if Assigned(Item) then
        repeat Item:=Item.Next; Inc(Result);
        until  Item=First;
     end;
end;

procedure   THashTable.Clear;
begin
  Reallocate(0);
end;

procedure   THashTable.Delete(var Item__:THashTableItem);
var Index:Integer;

  procedure ShrinkTable; {///$DEFINE VERIFY_MERGE}
  var BuddyBucketIndex, {$IFDEF VERIFY_MERGE} ItemCount, {$ENDIF}
      NewBucketCount:Integer; BuddyFirst,First,Item,Next,This:THashTableItem;
  begin
    if (ActivatedBucketCount>1) or (Count=0) then begin
       Dec(ActivatedBucketCount);

       if ActivatedBucketCount<=Succ(HashBitMask) div 2 then begin // 'True': the hash-bitmask is too big now and needs adjustment
          HashBitMask:=HashBitMask div 2;
          end;

       BuddyBucketIndex:=ActivatedBucketCount mod Succ((HashBitMask div 2)); // 'buddy' bucket of the bucket that is about to be pruned

       //if ActivatedBucketCount<=50 then Writeln(ActivatedBucketCount,SPACE,BuddyBucketIndex,' :: ',Succ(HashBitMask));

       {$IFDEF VERIFY_MERGE}
         Inc(ActivatedBucketCount);
         ItemCount:=BucketSize(Pred(ActivatedBucketCount))+BucketSize(BuddyBucketIndex); // calculate the number of items in the 2 buckets
         Dec(ActivatedBucketCount);
       {$ENDIF}

       BuddyFirst:=Buckets^[BuddyBucketIndex]; This:=BuddyFirst; // the first item in the 'buddy' bucket
       First:=Buckets^[ActivatedBucketCount]; Item:=First; // first item in the now unused bucket
       if   Assigned(BuddyFirst) then begin
            if Assigned(First) then // 'True:' the now unused bucket isn't empty; merge its items  with the 'buddy' bucket items
               repeat Next:=Item.Next; // remember the next item; otherwise, this information isn't available at the end of the loop
                      if   Item.HashKey<BuddyFirst.Prev.HashKey then // 'True': 'Item' isn't a new last item in the merged bucket
                           if      Item.HashKey>=This.HashKey then
                                   repeat This:=This.Next;
                                   until  Item.HashKey<This.HashKey  // sort the items in ascending order on hash-key; the loop is guaranteed to terminate because of the guard against 'Item' being a new last item
                           else if This=BuddyFirst then begin // 'True': new first item in the chain
                                   BuddyFirst:=Item; Buckets^[BuddyBucketIndex]:=Item;
                                   end
                                else begin
                                   end
                      else This:=BuddyFirst; // 'This' must be the next item, and since it's a circular list, the next item is the root item
                      Item.Prev:=This.Prev; Item.Next:=This; // insert the new item at its proper position in the chain
                      Item.Prev.Next:=Item; Item.Next.Prev:=Item; // update neighboring items in the chain
                      Item:=Next;
               until  Item=First;
            end
       else Buckets^[BuddyBucketIndex]:=First; // the 'buddy' bucket is empty; move all the now unused bucket to the 'buddy' bucket
       Buckets^[ActivatedBucketCount]:=nil; // clear the now unused bucket

       if ActivatedBucketCount<AllocatedBucketCount div 2 then begin // 'True': shrink the allocated bucket vector
          NewBucketCount:=3*(AllocatedBucketCount div 4); // shrink conservatively to 75%; that way, a new burst of additions doesn't require an immidiate expansion
          if (NewBucketCount>=Succ(HashBitMask)) and
             SafeRealloc(Pointer(Buckets),SizeOf(Buckets^[Low(Buckets^)])*AllocatedBucketCount,SizeOf(Buckets^[Low(Buckets^)])*NewBucketCount,False,True) then begin
             //Writeln('Shrink ',AllocatedBucketCount,' => ',NewBucketCount);
             AllocatedBucketCount:=NewBucketCount;
             Inc(HashTableResizeCount);
             end;
          end;

       {$IFDEF VERIFY_MERGE}
         // test that merging was implemented correctly; disable it in the production version
         if ActivatedBucketCount>0 then begin // 'True': the table isn't empty
            First:=Buckets^[BuddyBucketIndex]; Item:=First;
            if Assigned(Item) then
               repeat if (Item.Next<>First) and (Item.HashKey>Item.Next.HashKey) then
                         raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['THashTable.Remove: Items not sorted in ascending order.']));
                      Item:=Item.Next;
               until  Item=First;
            if ItemCount<>BucketSize(BuddyBucketIndex) then
               raise Exception.Create(Format(TEXT_INTERNAL_ERROR_FORMAT,['THashTable.Remove: Items were lost during shrinking.'])+SPACE+IntToStr(ItemCount-BucketSize(BuddyBucketIndex)));
            end;
       {$ENDIF}
       end;
  end;

begin // Delete; precondition: 'Item__' is a member of the table
  with Item__ do begin
    if   Next<>Item__ then begin // 'True': 'Item__' isn't the only item in the bucket
         Index:=BucketIndex(HashKey);
         if Item__=Buckets[Index] then Buckets[Index]:=Next; // update the bucket's root item
         Next.Prev:=Prev; // update neighboring items in the chain
         Prev.Next:=Next; // update neighboring items in the chain
         end
    else Buckets[BucketIndex(HashKey)]:=nil; // removing the last item in the bucket
    end;
  Dec(fCount);
  Item__.Free; Item__:=nil; // 'Item__' is a var-parameter (i.e., a reference-parameter); setting it to 'nil' helps the caller to avoid referencing the destroyed item later

  if Count<=(ActivatedBucketCount-HASH_TABLE_SHRINK_BUCKET_COUNT_THRESHOLD) shl LoadFactorPowerOf2Exponent then
     ShrinkTable;
end;

function    THashTable.ForEach(ForEachFunction__:THashTableForEachFunction):Integer;
var Index:Integer; Item,Next:THashTableItem;
begin
  Result:=0; Index:=ActivatedBucketCount;
  if (Count>0) and Assigned(ForEachFunction__) then
     repeat Dec(Index);
            if Index<ActivatedBucketCount then begin
               Item:=Buckets[Index];
               if Assigned(Item) then begin
                  repeat
                    Next:=Item.Next;
                    if   ForEachFunction__(Item) then begin
                         Inc(Result);
                         if      BucketIndex(Next.HashKey)<>Index then begin // 'True': the next item moved, e.g., 'Item' was deleted and the number of buckets changed
                                 Index:=Succ(ActivatedBucketCount); Next:=nil; // restart the iteration
                                 end
                         else if Next=Buckets[Index] then // 'True': all items in the bucket has been visited
                                 Next:=nil;
                         end
                    else begin Next:=nil; Index:=Low(Buckets^); // terminate loop
                         end;
                    Item:=Next;
                  until Next=nil;
                  end;
               end;
     until  Index=Low(Buckets^);
end;

function    THashTable.GetCapacity:Integer;
begin
  Result:=ActivatedBucketCount shl LoadFactorPowerOf2Exponent;
end;

function    THashTable.GetLoadFactor:Integer;
begin // the loadfactor is stored as the integer exponent 'n' in a '2^n' number
  Result:=PowerOf2(LoadFactorPowerOf2Exponent);
end;

function    THashTable.Lookup(HashKey__:THashTableHashKey; var Item__:THashTableItem):Boolean;
var First,This:THashTableItem;
begin
  Result:=False;
  if Assigned(Buckets) then begin
     First:=Buckets^[BucketIndex(HashKey__)];
     if Assigned(First) then begin
        This:=First;
        repeat if   This.HashKey>=HashKey__ then begin // the items are sorted in ascending order, hence, stop if the searched key is bigger than the one found in the current item
                    Result:=This.HashKey=HashKey__;
                    if Result then Item__:=This;
                    exit; // quick and dirty exit
                    //This:=First; // terminate the loop
                    end
               else This:=This.Next;
        until  This=First;
        end;
     end;
end;

function    THashTable.Reallocate(Capacity__:Integer):Boolean;
var Index,OldBucketCount,NewBucketCount,NewBucketsByteSize:Integer;
    Item,First,Next:THashTableItem;
    OldBuckets,NewBuckets:PHashTableItemVector;
begin // resizes the hash table, rehashing all items; special: 'Capacity__' = 0 clears the table
  Result:=True;
  NewBuckets:=nil;
  if Capacity__<0 then Capacity__:=0;
  NewBucketCount:=Capacity__ div LoadFactor;
  if   NewBucketCount<=MAX_HASH_TABLE_BUCKET_COUNT div 2 then
       if   NewBucketCount<>0 then // 'True': 'Capacity__' >= 'LoadFactor'
            NewBucketCount:=PowerOf2(CeilingLog2(NewBucketCount)) // rounded upwards to a power of 2
       else NewBucketCount:=Min(1,Capacity__) // 'Capacity__' < 'LoadFactor', possibly 0
  else NewBucketCount:=MAX_HASH_TABLE_BUCKET_COUNT; // round up to the maximum number of buckets (which is a 2^n number, where 'n' in an integer >= 0)
  Capacity__:=NewBucketCount * LoadFactor;

  if NewBucketCount<>AllocatedBucketCount then begin
     NewBucketsByteSize:=SizeOf(NewBuckets^[Low(NewBuckets^)])*NewBucketCount;
     if NewBucketsByteSize>0 then
        Result:=SafeGetMem(Pointer(NewBuckets),NewBucketsByteSize,True,False);
     if Result then begin
        OldBuckets:=Buckets; OldBucketCount:=ActivatedBucketCount;
        ActivatedBucketCount:=NewBucketCount;
        AllocatedBucketCount:=NewBucketCount; // extra buckets above 'ActivatedBucketCount' are allocated during expansion in the hope that it reduces the number of reallocations of the buckets
        Buckets:=NewBuckets;
        HashBitMask:=Pred(ActivatedBucketCount); // the hash-bitmask is a (2^n - 1) number
        Inc(HashTableResizeCount);
        fCount:=0; // '0': empty table
        if Assigned(OldBuckets) then begin
           for Index:=0 to Pred(OldBucketCount) do begin // rehash all existing items
               First:=OldBuckets[Index];
               if First<>nil then begin
                  Item:=First;
                  repeat Next:=Item.Next;
                         if   Capacity__<>0 then
                              Add(Item)
                         else Item.Free; // 'Capacity__' = 0 : clear the table
                         Item:=Next;
                  until  Item=First;
                  end;
               end;
           FreeMem(OldBuckets); // free the memory allocated for the old buckets
           end;
        end;
     end;
end;

procedure   THashTable.SetCapacity(Capacity__:Integer);
begin // as opposed to changing the loadfactor, changing the capacity restructures the table immidiately
  Reallocate(Capacity__);
end;

procedure   THashTable.SetLoadFactor(LoadFactor__:Integer);
begin
  // changing the loadfactor doesn't immidiately restructure the table, but future transactions take the new factor into account;
  // the loadfactor is stored as the exponent 'n' in a '2^n' number where 'n' is an integer >= 0, (the loadfactor is truncated towards 0 so it never exceeds the value specified by the caller)
  LoadFactorPowerOf2Exponent:=Log2(Min(High(LoadFactor) div 2,Max(1,LoadFactor__)));
  while MultiplicationOverflow(ActivatedBucketCount,PowerOf2(LoadFactorPowerOf2Exponent),High(Integer)) do
    Dec(LoadFactorPowerOf2Exponent); // guard the capacity against overflow; Capacity = 'ActivatedBucketCount' * 'LoadFactor'
end;

function    THashTable.ShowBucket(Index__:Integer):Integer;
var First,Item:THashTableItem;
begin
  Result:=0;
  Writeln('Bucket ',Index__,COLON);
  if (Index__>=Low(Buckets^)) and (Index__<ActivatedBucketCount) then begin
     First:=Buckets^[Index__]; Item:=First;
     if Assigned(Item) then
        repeat Writeln('  ',Item.ToString);
               Item:=Item.Next; Inc(Result);
        until  Item=First;
     end;
  Writeln(Index__,COLON,SPACE,Result,' item(s)');
end;

function    THashTable.Stats:Integer;
var i,n,MaxBucketSize:Integer; Stats:array[0..1000] of Integer;
begin
  FillChar(Stats,SizeOf(Stats),0);
  MaxBucketSize:=0;
  for i:=Low(Buckets^) to Pred(ActivatedBucketCount) do begin
      n:=BucketSize(i);
      if n>MaxBucketSize then MaxBucketSize:=n;
      Inc(Stats[Min(n,High(Stats))]);
      end;

  Writeln; n:=0;
  for i:=Low(Stats) to High(Stats) do
      if n<Count then begin
         Inc(n,i*Stats[i]);
         if   Stats[i]=0 then
              Writeln(i:4,COLON,SPACE,'':8,n:10)
         else Writeln(i:4,COLON,SPACE,Stats[i]:8,n:10);
         end;

  Writeln;
  Writeln('Max. bucket size: ',MaxBucketSize);
  Writeln('ActiveBuckets   : ',ActivatedBucketCount);

  Result:=Count;
end;

////////////////////////////////////////////////////////////////////////////////
// TStringKeyHashTable - hash tables where keys are strings
////////////////////////////////////////////////////////////////////////////////

function    TStringKeyHashTable.Lookup(const Key__:String; var Item__:TStringKeyHashTableItem):Boolean;
var HashKey:THashTableHashKey; Last:TStringKeyHashTableItem;
begin
  Result:=False;
  HashKey:=StrHashValuePJW(Key__);
  if inherited Lookup(HashKey,THashTableItem(Item__)) then
     if Key__=Item__.Key then
        Result:=True
     else begin
        Last:=TStringKeyHashTableItem(Item__.Prev);
        if Last<>Item__ then  // 'True': this bucket contains more than 1 item
           repeat Item__:=TStringKeyHashTableItem(Item__.Next);
                  if Item__.Key=Key__ then begin
                     Result:=True; exit; // found: quick and dirty exit
                     end
           until  (Item__.HashKey<>HashKey) or (Item__=Last); // until all items with the same hash-key, or all items in the bucket, have been visited
        end;
end;

////////////////////////////////////////////////////////////////////////////////
// End of hash-table implementation
////////////////////////////////////////////////////////////////////////////////

initialization
  InitializeHashTables;
end.
