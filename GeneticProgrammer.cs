using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Diagnostics;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.InteropServices;
using System.Text.RegularExpressions;
using System.Threading;

namespace Bugger
{

#region CodeGenerator
public sealed class CodeGenerator
{
  public CodeGenerator(ILGenerator ilg, Label[] jumpLabels)
  {
    this.ilg = ilg;
    this.jumpLabels = jumpLabels;
  }

  public ILGenerator ILG
  {
    get { return ilg; }
  }

  public LocalBuilder AllocTemp(Type type)
  {
    if(temps == null) temps = new List<LocalBuilder>();

    for(int i=temps.Count-1; i >= 0; i--)
    {
      if(temps[i].LocalType == type)
      {
        LocalBuilder temp = temps[i];
        temps.RemoveAt(i);
        return temp;
      }
    }

    return ilg.DeclareLocal(type);
  }

  public void FreeTemp(LocalBuilder temp)
  {
    temps.Add(temp);
  }

  public void EmitInterrupt()
  {
    if(jumpIndex == jumpLabels.Length) throw new InvalidOperationException("Too many interrupts.");

    FieldInfo stateField = typeof(IndividualContext).GetField("State", BindingFlags.NonPublic|BindingFlags.Instance);
    ILG.Emit(OpCodes.Ldarg_0);
    if(jumpIndex < 127) ILG.Emit(OpCodes.Ldc_I4_S, (byte)jumpIndex);
    else ILG.Emit(OpCodes.Ldc_I4, jumpIndex);
    ILG.Emit(OpCodes.Stfld, stateField);
    ILG.Emit(OpCodes.Ret);
    ILG.MarkLabel(jumpLabels[jumpIndex++]);
  }

  internal bool ConsumedAllJumps
  {
    get { return jumpIndex == jumpLabels.Length; }
  }

  readonly ILGenerator ilg;
  readonly Label[] jumpLabels;
  List<LocalBuilder> temps;
  int jumpIndex;
}
#endregion

#region CreationSize
/// <summary>Determines how much code is created when a new, random individual is formed.</summary>
[Flags]
public enum CreationSize
{
  /// <summary>Fills the tree to one quarter its maximum size.</summary>
  Quarter=1,
  /// <summary>Fills the tree to half its maximum size.</summary>
  Half=2,
  /// <summary>Fills the tree to its maximum size.</summary>
  Full=3,
  /// <summary>A mask that can be applied to get the size -- Quarter, Half, or Full.</summary>
  SizeMask=3,
  /// <summary>Fills the tree to a random number between one sixteenth and the size specified by the size mask.</summary>
  Random=4
}
#endregion

#region Frame
[StructLayout(LayoutKind.Sequential, Pack=4)]
public struct Frame
{
  public Frame(byte ip) { Context=null; State=0; IP=ip; }
  public Frame(byte ip, short state) { Context=null; State=state; IP=ip; }
  public Frame(byte ip, short state, object context) { Context=context; State=state; IP=ip; }

  public override string ToString()
  {
    return string.Format("{0} {1} {2}", IP, State, Context);
  }

  /// <summary>An arbitrary context value.</summary>
  public readonly object Context;
  /// <summary>An arbitrary state value, typically used to determine where to begin executing when a function is
  /// called.
  /// </summary>
  public readonly short State;
  /// <summary>The IP to jump to to execute the current frame.</summary>
  public readonly byte IP;
}
#endregion

#region FrameStack
/// <summary>Represents a stack of <see cref="Frame"/> within a portion of an array.</summary>
public sealed class FrameStack
{
  internal FrameStack(Frame[] frames)
  {
    if(frames == null) throw new ArgumentNullException();
    this.frames = frames;
    SetRegion(0, frames.Length, 0);
  }

  public int Count
  {
    get { return count; }
  }

  public Frame Last
  {
    get { return lastFrame; }
  }

  public bool IsEmpty
  {
    get { return count == 0; }
  }

  public void Push(byte ip) { Push(new Frame(ip)); }
  public void Push(byte ip, short state) { Push(new Frame(ip, state)); }
  public void Push(byte ip, short state, object context) { Push(new Frame(ip, state, context)); }

  public void Push(Frame frame)
  {
    if(count == maxCount) throw new InvalidOperationException("Stack overflow.");
    frames[startIndex + count++] = frame;
  }

  public void Pop()
  {
    if(count == 0) throw new InvalidOperationException("Stack underflow.");
    lastFrame = frames[startIndex + --count];
  }

  /// <summary>Sets the region of the frame array that's valid for use as a stack.</summary>
  internal void SetRegion(int index, int length, int count)
  {
    if(index < 0 || length < 0 || index+length > frames.Length || count < 0 || count > length)
    {
      throw new ArgumentOutOfRangeException();
    }

    this.startIndex = index;
    this.maxCount   = length;
    this.count      = count;
  }

  readonly Frame[] frames;
  Frame lastFrame;
  int startIndex, maxCount, count;
}
#endregion

#region Function
public abstract class Function<T> where T : struct, IIndividual
{
  protected Function(string name, int arity, int chance)
  {
    if(string.IsNullOrEmpty(name)) throw new ArgumentException();
    if(arity < 0 || chance <= 0) throw new ArgumentOutOfRangeException();

    this.name   = name;
    this.arity  = arity;
    this.chance = chance;
  }

  internal delegate void Delegate(GeneticProgrammer<T> GP, ref T individual, int thread);

  public struct EmitArgument
  {
    public EmitArgument(Function<T> function, EmitArgument[] args) { Function = function; Args = args; }
    public readonly Function<T> Function;
    public readonly EmitArgument[] Args;
  }
  
  /// <summary>Gets the number of arguments this method accepts.</summary>
  public int Arity
  {
    get { return arity; }
  }

  /// <summary>Gets the chance that a call to this method will be generated. The chance is evaluated relative to other
  /// function chances.
  /// </summary>
  public int Chance
  {
    get { return chance; }
  }

  /// <summary>Gets the name of this function.</summary>
  public string Name
  {
    get { return name; }
  }

  /// <summary>Gets whether this function, when compiled, interrupts execution of the compiled method.</summary>
  /// <remarks>The default implementation returns true if <see cref="Arity"/> is zero and false otherwise.</remarks>
  public virtual bool InterruptsExecution
  {
    get { return arity == 0; }
  }

  /// <summary>Implements this function.</summary>
  /// <param name="stack">The execution stack.</param>
  /// <param name="individual">The individual currently running.</param>
  /// <param name="args">The child nodes representing the arguments to this function. These should be used as
  /// opaque values to construct new stack frames to call the child nodes. Only the arguments at indices 0 to
  /// <see cref="Arity"/>-1 are valid to use, however.
  /// </param>
  /// <param name="thread">The index of the thread on which the function is being called.</param>
  /// <returns>Returns true if execution should continue and false if execution should be interrupted.</returns>
  public abstract bool Call(FrameStack stack, ref T individual, byte[] args, int thread);

  /// <summary>Instructs this function to emit itself.</summary>
  /// <param name="ilGen">The <see cref="GeneticProgrammer{T}.CodeGenerator"/> to which the code should be emitted.</param>
  /// <param name="args">A list of arguments to this function.</param>
  /// <remarks>The 'this' pointer (the first argument) is a reference to a
  /// <see cref="GeneticProgrammer{T}.IndividualContext"/> object. The second argument is a reference to the
  /// The third argument is a reference to the <see cref="GeneticProgrammer{T}"/> object.
  /// individual (and object of type <typeparamref name="T"/>). The fourth argument is the index of the thread.
  /// The default implementation throws a <see cref="NotSupportedException"/>.
  /// </remarks>
  public virtual void Emit(CodeGenerator codeGen, EmitArgument[] args)
  {
    throw new NotSupportedException();
  }

  public override string ToString()
  {
    return arity == 0 ? name : name+"`"+arity;
  }

  readonly string name;
  readonly int arity, chance;
}
#endregion

#region IIndividual
/// <summary>Represents the basic interface to an individual within a population.</summary>
public interface IIndividual
{
  /// <summary>Gets whether an individual has died or otherwise completed its task.</summary>
  /// <remarks>This is evaluated in all tournament modes after each interruptible action the individual takes.</remarks>
  bool IsDead { get; }

  /// <summary>Used in all tournament modes to calculate the fitness of an individual.</summary>
  /// <returns>Returns the fitness as a value from 0 to 1, with higher fitnesses being better.</returns>
  double GetFitness();
}
#endregion

#region IndividualContext
public sealed class IndividualContext
{
  public IndividualContext()
  {
    State = -1;
  }

  public void Push(object obj)
  {
    if(stack == null) stack = new Stack<object>();
    stack.Push(obj);
  }

  public object Pop()
  {
    return stack.Pop();
  }

  Stack<object> stack;
  internal int State;
}
#endregion

#region TournamentMode
/// <summary>Determines how the tournament will operate.</summary>
public enum TournamentMode
{
  /// <summary>A round consists of each individual running to completion independently of the others. Each round
  /// results in the creation of an entire set of new individuals. Use this mode when individuals cannot interact.
  /// </summary>
  Generational,
  /// <summary>A round consists of each individual running until an interrupt point, at which time other individuals
  /// are given a chance to run. Use this mode when individuals can interact with each other.
  /// </summary>
  Ongoing
}
#endregion

#region GeneticProgrammer
public class GeneticProgrammer<T> : IDisposable where T : struct, IIndividual
{
  public const int MaxMaxCodeSize = 255, MaxAverageGenerations = 255;
  const int MaxFunctions = 255;

  ~GeneticProgrammer() { Dispose(true); }

  public CreationSize CreationSize
  {
    get { return creationSize; }
    set { creationSize = value; }
  }

  /// <summary>Gets a collection of the individuals in the population.</summary>
  public ReadOnlyCollection<T> Individuals
  {
    get
    {
      AssertInitialized();
      return new ReadOnlyCollection<T>(individuals);
    }
  }

  /// <summary>The number of generations over which an individual's fitness will be evaluated.</summary>
  public int AverageGenerations
  {
    get { return ageThreshold; }
    set
    {
      if(value < 1 || value > MaxAverageGenerations) throw new ArgumentOutOfRangeException();
      ageThreshold = value;
    }
  }

  /// <summary>Gets whether individuals' code will be compiled.</summary>
  public bool Compiled
  {
    get { return compiledCode != null; }
  }

  /// <summary>The percentage chance of an individual being generated via crossover.</summary>
  public int CrossoverChance
  {
    get { return crossoverChance; }
    set
    {
      if(value < 0 || value > 100) throw new ArgumentOutOfRangeException();
      crossoverChance = value;
    }
  }

  /// <summary>The percentage chance of an individual being generated via mutation.</summary>
  public int MutationChance
  {
    get { return mutationChance; }
    set
    {
      if(value < 0 || value > 100) throw new ArgumentOutOfRangeException();
      mutationChance = value;
    }
  }

  /// <summary>Gets the maximum code size allowed.</summary>
  public int MaxCodeSize
  {
    get { return maxCodeSize; }
  }

  /// <summary>Gets or sets a fitness threshold at or below which creatures have a chance of being completely replaced
  /// with new, random code.
  /// </summary>
  public double ReplacementThreshold
  {
    get { return replacementThreshold; }
    set
    {
      if(value < 0 || value > 1) throw new ArgumentOutOfRangeException();
      replacementThreshold = value;
    }
  }

  /// <summary>Gets the number of rounds that have run so far.</summary>
  public int Rounds
  {
    get { return rounds; }
  }

  /// <summary>Gets the number of threads used to execute the simulation.</summary>
  public int Threads
  {
    get { return threads == null ? 1 : threads.Length+1; }
  }

  /// <summary>Gets the tournament mode used to initialize this genetic programmer.</summary>
  public TournamentMode TournamentMode
  {
    get
    {
      AssertInitialized();
      return mode;
    }
  }

  public void Dispose()
  {
    GC.SuppressFinalize(this);
    Dispose(false);
  }

  /// <summary>Returns a LISP representation of the code of the given individual.</summary>
  public string GetCode(int individual)
  {
    AssertInitialized();
    if(individual < 0 || individual >= individuals.Length) throw new ArgumentOutOfRangeException();
    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    GetCode(sb, individual*maxCodeSize, 0);
    return sb.ToString();
  }

  /// <summary>Sets an individual's code given a LISP representation of it.</summary>
  public void SetCode(int individual, string code, bool randomizeIndividual)
  {
    CodePoint[] buffer = new CodePoint[maxCodeSize];
    int bufferIndex = 0;
    Match m = lispRe.Match(code);
    ParseCode(ref m, buffer, ref bufferIndex);
    Array.Copy(buffer, 0, this.code, individual*maxCodeSize, buffer[0].TreeSize);
    OnCodeReset(individual);
    if(randomizeIndividual) GenerateRandomIndividual(individual, out individuals[individual]);
  }

  void ParseCode(ref Match m, CodePoint[] buffer, ref int bufferIndex)
  {
    if(!m.Success || m.Value != "(")
    {
      int badIndex = m.Success ? m.Index : -bufferIndex;
      throw new ArgumentException("Expected '(' at index "+badIndex.ToString());
    }

    m = m.NextMatch();
    if(!m.Success || m.Value == "(" || m.Value == ")")
    {
      int badIndex = m.Success ? m.Index : -bufferIndex;
      throw new ArgumentException("Expected function name at index "+badIndex.ToString());
    }

    bool found = false;
    for(int fi=0; fi<functions.Length; fi++)
    {
      if(functions[fi].Name == m.Value)
      {
        int startIndex = bufferIndex;
        buffer[bufferIndex++].Function = (byte)fi;
        m = m.NextMatch();
        for(int i=0; i<functions[fi].Arity; i++)
        {
          ParseCode(ref m, buffer, ref bufferIndex);
        }
        buffer[startIndex].TreeSize = (byte)(bufferIndex-startIndex);
        found = true;
        break;
      }
    }
    if(!found) throw new ArgumentException("No such function '"+m.Value+"'");

    if(!m.Success || m.Value != ")")
    {
      int badIndex = m.Success ? m.Index : -bufferIndex;
      throw new ArgumentException("Expected ')' at index "+badIndex.ToString());
    }

    m = m.NextMatch();
  }

  /// <summary>Returns the size of the code associated with the given individual.</summary>
  public int GetCodeSize(int individual)
  {
    return GetTreeSize(individual*maxCodeSize);
  }
  
  /// <summary>Executes a single round.</summary>
  public virtual void Run()
  {
    // first, check for and replace dead individuals
    for(int i=0; i<individuals.Length; i++)
    {
      if(individuals[i].IsDead)
      {
        ReplaceDeadIndividuals();
        break;
      }
    }

    if(threadWakeup != null) // if we're doing multi-threading, start the other threads
    {
      threadsDone = 0;
      threadSleep.Reset();
      threadWakeup.Set();
    }

    // then simulate all individuals in the first thread
    Exception exception = null;
    try
    {
      for(int i=0, end=(threads == null ? individuals.Length : threads[0].Start); i<end; i++)
      {
        RunIndividual(i, 0);
      }
    }
    catch(Exception ex)
    {
      exception = ex;
    }

    if(threadWakeup != null)
    {
      while(threadsDone != threads.Length) Thread.Sleep(0);
      threadWakeup.Reset();
      threadSleep.Set();
      while(threadsDone != 0) Thread.Sleep(0);
    }

    rounds++;

    if(threads != null)
    {
      for(int i=0; i<threads.Length; i++)
      {
        if(threads[i].Exception != null)
        {
          exception = new ApplicationException("Exception occurred on thread "+(i+1).ToString(), threads[i].Exception);
          break;
        }
      }
    }

    if(exception != null) throw exception;
  }

  /// <summary>Initializes the genetic programmer.</summary>
  /// <param name="maxIndividuals">The maximum number of individuals that can be alive at once. (In generational mode,
  /// this is the number of individuals in each generation).
  /// </param>
  /// <param name="maxCodeSize">The maximum number of code points each individual can have.</param>
  public virtual void Initialize(TournamentMode mode, int maxIndividuals, int maxCodeSize, int maxStackDepth,
                                 int numThreads, bool compiled, IEnumerable<Function<T>> functions)
  {
    if(functions == null) throw new ArgumentNullException();
    if(maxIndividuals <= 0 || maxStackDepth <= 0 || maxCodeSize > MaxMaxCodeSize || numThreads < 1)
    {
      throw new ArgumentOutOfRangeException();
    }

    this.maxCodeSize    = maxCodeSize;
    this.maxStackDepth  = maxStackDepth;
    this.mode           = mode;
    this.rounds         = 0;
    this.individuals    = new T[maxIndividuals];
    this.code           = new CodePoint[maxIndividuals*maxCodeSize];
    this.averageFitness = new double[maxIndividuals];
    this.age            = new byte[maxIndividuals];
    this.nextOperation  = 0;

    // create the threads
    numThreads = Math.Min(numThreads, individuals.Length); // we won't have more threads than individuals
    DisposeThreads();
    if(numThreads > 1)
    {
      threads      = new SimulationThread[numThreads-1];
      threadWakeup = new ManualResetEvent(false);
      threadSleep  = new ManualResetEvent(false);
      for(int i=0, nTotal=0, nPerThread=maxIndividuals/numThreads; i<threads.Length; i++)
      {
        nTotal += nPerThread;
        threads[i].Start  = nTotal;
        threads[i].Count  = Math.Max(nPerThread, maxIndividuals-nTotal);
        threads[i].Thread = new Thread(ThreadFunc);
        threads[i].Thread.Start(i);
      }
    }

    // set up the execution environment (stacks, values, etc)
    if(compiled)
    {
      this.frameStacks  = null;
      this.stackDepths  = null;
      this.compiledCode = new Function<T>.Delegate[maxIndividuals];
    }
    else
    {
      this.compiledCode = null;
      this.frameStacks  = new FrameStack[numThreads];

      if(mode == TournamentMode.Generational)
      {
        for(int i=0; i<numThreads; i++)
        {
          this.frameStacks[i] = new FrameStack(new Frame[maxStackDepth]);
        }
        this.stackDepths = null;
      }
      else if(mode == TournamentMode.Ongoing)
      {
        this.frameStacks[0] =
          new FrameStack(new Frame[maxStackDepth*(threads==null ? maxIndividuals : threads[0].Start)]);

        for(int i=0; i<numThreads-1; i++)
        {
          this.frameStacks[i+1] = new FrameStack(new Frame[maxStackDepth*threads[i].Count]);
        }

        this.stackDepths = new byte[maxIndividuals];
      }
      else throw new ArgumentOutOfRangeException("mode");
    }

    // group functions by arity and check for errors
    Dictionary<int,List<Function<T>>> arityMap = new Dictionary<int,List<Function<T>>>();
    Dictionary<string, Function<T>> nameMap = new Dictionary<string, Function<T>>();
    int maxArity = 0;
    foreach(Function<T> function in functions)
    {
      if(function == null) throw new ArgumentException("A function was null.");
      if(nameMap.ContainsKey(function.Name)) throw new ArgumentException("A function name was duplicated.");
      nameMap[function.Name] = null;
      
      List<Function<T>> list;
      if(!arityMap.TryGetValue(function.Arity, out list)) arityMap[function.Arity] = list = new List<Function<T>>();
      list.Add(function);
      if(function.Arity > maxArity) maxArity = function.Arity;
    }

    if(arityMap.Count == 0) throw new ArgumentException("No functions were given.");
    this.arguments = new byte[maxArity];

    // initialize arity choosers
    arityChoosers = new RandomChooser[arityMap.Count];
    int index = 0, count = 0;
    foreach(List<Function<T>> arityFuncs in arityMap.Values)
    {
      int totalChance = 0;
      foreach(Function<T> function in arityFuncs) totalChance += function.Chance;

      // set the index to the first function with the given arity
      arityChoosers[index].NextIndex   = count;
      arityChoosers[index].TotalChance = totalChance;
      index++;
      count += arityFuncs.Count;
    }

    if(count > MaxFunctions) throw new ArgumentException("Too many functions were given.");

    // store the functions, grouped by arity
    this.functions = new Function<T>[count];
    index = 0;
    foreach(List<Function<T>> arityFuncs in arityMap.Values)
    {
      arityFuncs.CopyTo(this.functions, index);
      index += arityFuncs.Count;
    }

    // set the global function chooser
    count = 0;
    foreach(RandomChooser rc in arityChoosers) count += rc.TotalChance;
    functionChooser.NextIndex   = 0;
    functionChooser.TotalChance = count;

    // mark initialized
    initialized = true;

    GenerateRandomPopulation();
  }

  #region TreeWalker
  sealed class TreeWalker : IEnumerator<CodePoint>
  {
    public TreeWalker(CodePoint[] code, int rootIndex, Function<T>[] functions, int maxCodeSize)
    {
      this.functions = functions;
      this.code  = code;
      this.root  = rootIndex;
      this.stack = new StackItem[maxCodeSize];
      this.depth = this.index = -1;
    }

    public byte Function
    {
      get { return Current.Function; }
    }

    public int Index
    {
      get
      {
        if(depth < 0) throw new InvalidOperationException();
        return index;
      }
    }

    public byte Offset
    {
      get { return (byte)(Index-root); }
    }

    public int Root
    {
      get { return root; }
    }

    public byte TreeSize
    {
      get { return Current.TreeSize; }
    }

    public CodePoint Current
    {
      get
      {
        if(depth < 0) throw new InvalidOperationException();
        return current;
      }
    }

    public bool MoveNext()
    {
      if(index == -1) // if we're at BOF, advance to the root
      {
        depth++;
        index   = root;
        current = code[index];
        stack[depth] = new StackItem(0, (byte)functions[current.Function].Arity);
        return true;
      }
      else if(depth == -1) return false; // we're at EOF, return false

      // we're at an arbitrary node and we need to move to its children, or its sibling, or its parent's sibling, etc.

      // if we've finished all the children in this node, go back up the tree until we find more
      if(stack[depth].Child == stack[depth].Arity)
      {
        do
        {
          if(--depth < 0) return false;
        } while(stack[depth].Child == stack[depth].Arity);

        index = root+stack[depth].Offset;
        stack[depth].Offset += code[index].TreeSize;
        index += code[index].TreeSize;
      }
      else if(stack[depth].Child == 0) // it's the first time to move down into this node's children
      {
        stack[depth].Offset++;
        index++;
      }

      // we've got more siblings in the current node. move to the next one
      stack[depth].Child++;
      current = code[index];
      stack[++depth] = new StackItem((byte)(index-root), (byte)functions[current.Function].Arity);
      return true;
    }

    public bool MoveTo(int index)
    {
      if(this.index != index || depth == -1)
      {
        while(MoveNext() && this.index != index) { } // move to the destination index
      }
      return this.index == index;
    }

    public bool MoveToParent()
    {
      if(depth == -1 || --depth < 0) return false;

      stack[depth].Offset = stack[depth].OriginalOffset;
      stack[depth].Child  = 0;
      index = root + stack[depth].OriginalOffset;
      current = code[index];

      return true;
    }

    public void Reset()
    {
      index  = depth = -1;
      current = new CodePoint();
    }

    [StructLayout(LayoutKind.Sequential, Pack=2)]
    struct StackItem
    {
      public StackItem(byte offset, byte arity) { OriginalOffset=Offset=offset; Child=0; Arity=arity; }
      public byte OriginalOffset, Offset, Child, Arity;
    }

    object System.Collections.IEnumerator.Current
    {
      get { return Current; }
    }

    void IDisposable.Dispose() { }

    readonly CodePoint[] code;
    readonly Function<T>[] functions;
    readonly StackItem[] stack;
    readonly int root;
    int index, depth;
    CodePoint current;
  }
  #endregion

  protected struct IndividualFitness
  {
    public override string ToString()
    {
      return string.Format("{0} {1} ({2})", Age, Fitness, Individual);
    }

    /// <summary>The individual's fitness.</summary>
    public double Fitness;
    /// <summary>The individual.</summary>
    public int Individual;
    /// <summary>The individual's age.</summary>
    public byte Age;
  }

  /// <summary>Given a dead individual and its normalized fitness, choose an individual to clone to replace it.</summary>
  /// <returns>The index of an individual to clone, or -1 to generate a new individual.</returns>
  protected virtual int ChooseOneToClone(int individual, double fitness, IndividualFitness[] population)
  {
    if(population[0].Fitness <= 0.05) return -1; // if everyone sucks, generate a random individual

    // otherwise, choose somebody from the top 5% of the population
    double top5 = 0.95*population[0].Fitness + 0.05*population[population.Length-1].Fitness;
    int index = 0;
    while(index < population.Length && population[index].Fitness >= top5) index++;

    return population[rand.Next(index)].Individual;
  }

  protected virtual void ChooseParentsForCrossover(int individual, double fitness, IndividualFitness[] population,
                                                   out int mother, out int father)
  {
    // choose two parents from the top 10% of the population
    double top5 = 0.90*population[0].Fitness + 0.10*population[population.Length-1].Fitness;

    int index = 0;
    while(index < population.Length && population[index].Fitness > top5) index++;
    if(index == 0) index = population.Length;

    mother = population[rand.Next(index)].Individual;

    // choose a different father unless there's only one to choose from
    do father = population[rand.Next(index)].Individual;
    while(father == mother && index > 1);
  }

  /// <summary>Clones an individual, preserving the fitness and generation, and calling
  /// <see cref="InitializeFromClone"/> to initialize the individual.
  /// </summary>
  protected void CloneCode(int fromIndividual, int toIndividual)
  {
    // save the old fitness/age values because they'll be clobbered by OnCodeReset if individual == cloneFrom
    Function<T>.Delegate fromDelegate = compiledCode != null ? compiledCode[fromIndividual] : null;
    double fromFitness = averageFitness[fromIndividual];
    byte fromAge = age[fromIndividual];

    InitializeFromClone(toIndividual, out individuals[toIndividual], ref individuals[fromIndividual]);

    if(fromIndividual != toIndividual) CopyCode(fromIndividual, toIndividual);
    else OnCodeReset(toIndividual);

    averageFitness[toIndividual] = fromFitness;
    age[toIndividual] = (byte)(fromAge < MaxAverageGenerations ? fromAge+1 : fromAge);
    if(compiledCode != null) compiledCode[toIndividual] = fromDelegate;
  }

  /// <summary>Copies the code from one individual to another.</summary>
  protected void CopyCode(int fromIndividual, int toIndividual)
  {
    int srcIndex = fromIndividual*maxCodeSize;
    Array.Copy(code, srcIndex, code, toIndividual*maxCodeSize, code[srcIndex].TreeSize);
    OnCodeReset(toIndividual);
    if(compiledCode != null) compiledCode[toIndividual] = compiledCode[fromIndividual];
  }

  /// <summary>Replaces the given individual with a new individual created by combining code from the mother and father
  /// individuals.
  /// </summary>
  /// <returns>Returns true if the crossover was successful and false if not.</returns>
  /// <remarks>The destination individual's code may be overwritten even if the crossover was not ultimately
  /// successful.
  /// </remarks>
  protected bool CrossoverCode(int individual, int mother, int father)
  {
    // the new program will be the mother's program plus a piece of the father's program

    // we want to copy the mother's code to the individual, but if the individual is its own father, that would
    // overwrite the father's genetic information before we'd have a chance to grab a piece of it.
    // in that case, swap the father and mother, making mother == individual
    if(father == individual)
    {
      Debug.Assert(father != mother);

      int temp = father;
      father = mother;
      mother = father;
    }
    else if(mother != individual)
    {
      CopyCode(mother, individual);
    }

    // try three times to copy a piece of code from the father
    for(int tries=0; tries<3; tries++)
    {
      int srcIndex = SelectNodeForCrossover(father, 0),
         destIndex = SelectNodeForCrossover(individual, code[srcIndex].TreeSize);

      if(destIndex != -1 && srcIndex != destIndex) // if we could find a subtree of an appropriate size
      {
        int srcSize = code[srcIndex].TreeSize, destSize = code[destIndex].TreeSize;

        if(srcSize == destSize)
        {
          if(srcSize == 1) code[destIndex] = code[srcIndex];
          else Array.Copy(code, srcIndex, code, destIndex, srcSize);
        }
        else
        {
          int destRoot = individual*maxCodeSize, destBlockEnd = destIndex+destSize,
                toMove = destRoot + code[destRoot].TreeSize - destBlockEnd;
          int difference = srcSize - destSize;
          TreeWalker tw = GetTreeWalker(destRoot);
          tw.MoveTo(destIndex);

          int destRootSize = code[destRoot].TreeSize;
          Debug.Assert(toMove >= 0);
          Debug.Assert(code[destRoot].TreeSize + difference > 0 && destRootSize + difference <= maxCodeSize);

          CodePoint[] verify = new CodePoint[maxCodeSize];
          Array.Copy(code, destRoot, verify, 0, destIndex-destRoot);
          Array.Copy(code, srcIndex, verify, destIndex-destRoot, srcSize);
          Array.Copy(code, destBlockEnd, verify, destIndex-destRoot+srcSize, destRootSize - (destBlockEnd-destRoot));

          Array.Copy(code, destBlockEnd, code, destBlockEnd + difference, toMove);
          Array.Copy(code, srcIndex, code, destIndex, srcSize);

          for(int i=0; i<destRootSize+difference; i++)
          {
            Debug.Assert(verify[i].Equals(code[destRoot+i]));
          }

          // update the tree sizes of all the ancestors of the new destination node
          while(tw.MoveToParent())
          {
            Debug.Assert(tw.TreeSize + difference > 0 && tw.TreeSize + difference <= maxCodeSize);
            code[tw.Index].TreeSize = (byte)(tw.TreeSize + difference);
          }
        }

        Debug.Assert(GetTreeSize(father*maxCodeSize) == code[father*maxCodeSize].TreeSize);
        Debug.Assert(GetTreeSize(individual*maxCodeSize) == code[individual*maxCodeSize].TreeSize);

        OnCodeReset(individual);
        return true;
      }
    }

    return false;
  }

  protected virtual void Dispose(bool finalizing)
  {
    DisposeThreads();
  }

  /// <summary>Generates a new, random individual.</summary>
  protected virtual void GenerateRandomIndividual(int index, out T individual)
  {
    individual = new T();
  }

  /// <summary>Generates a new, random population.</summary>
  protected void GenerateRandomPopulation()
  {
    AssertInitialized();
    for(int i=0; i<individuals.Length; i++)
    {
      GenerateRandomIndividual(i, out individuals[i]);
      GenerateRandomCode(i);
    }
  }

  /// <summary>Generates a new, random program for the given individual and resets the individual's StackDepth.</summary>
  protected void GenerateRandomCode(int individual)
  {
    AssertInitialized();
    // create a new program by selecting random functions to call until a random code size is reached or all leaves
    // are terminal (0-arity) functions

    int maxSize;
    switch(creationSize&CreationSize.SizeMask)
    {
      case CreationSize.Quarter: maxSize = (maxCodeSize+3)/4; break;
      case CreationSize.Full: maxSize = maxCodeSize; break;
      case CreationSize.Half:
      default:
        maxSize = (maxCodeSize+1)/2;
        break;
    }

    int size = (creationSize&CreationSize.Random) == 0 ? maxSize : rand.Next((maxCodeSize+15)/16, maxSize+1);
    int index = individual*maxCodeSize;
    AddRandomCode(index, size);
    OnCodeReset(individual);
    Debug.Assert(GetTreeSize(index) == code[index].TreeSize);
  }

  /// <summary>Initializes a new individual given the individual (possibly itself) from which it was cloned.</summary>
  protected virtual void InitializeFromClone(int index, out T individual, ref T cloneOf)
  {
    GenerateRandomIndividual(index, out individual);
  }

  /// <summary>Initializes a new individual given another individual from its parents, which may be the same as itself.</summary>
  protected virtual void InitializeFromParents(int index, out T individual, ref T mother, ref T father)
  {
    GenerateRandomIndividual(index, out individual);
  }

  /// <summary>Initializes a new individual given its old self from which it was mutated.</summary>
  protected virtual void InitializeMutated(int index, ref T individual)
  {
    GenerateRandomIndividual(index, out individual);
  }

  /// <summary>Mutates an individual's code.</summary>
  protected void MutateCode(int individual)
  {
    int codeBase = individual*maxCodeSize;

    // we have a choice of things we can do: 1) rotate function arguments, 2) replace random subtree
    int index;
    if(rand.Next(100) < 50) // we'll rotate function arguments
    {
      index = SelectNodeForMutation(individual, 2, arguments.Length);
      if(index != -1)
      {
        int arity = functions[code[index].Function].Arity;

        // first, copy all subtrees to a buffer
        CodePoint[] buffer = new CodePoint[maxCodeSize];
        int[] lengths = new int[arity];
        for(int i=0, argIndex=index+1, writeIndex=0; i < arity; i++)
        {
          lengths[i] = code[argIndex].TreeSize;
          Array.Copy(code, argIndex, buffer, writeIndex, lengths[i]);
          writeIndex += lengths[i];
          argIndex   += lengths[i];
        }

        // then copy the subtrees back, offset by one (the first argument is read from the second tree in the buffer)
        for(int i=0, argIndex=index+1, readIndex=0; i < arity; i++)
        {
          int toRead;
          if(i == arity-1) // read the last item from the beginning
          {
            toRead    = lengths[0];
            readIndex = 0;
          }
          else
          {
            toRead     = lengths[i+1];
            readIndex += lengths[i];
          }

          Array.Copy(buffer, readIndex, code, argIndex, toRead);
          argIndex += toRead;
        }

        OnCodeReset(individual);
        Debug.Assert(GetTreeSize(codeBase) == code[codeBase].TreeSize);
        return;
      }
    }

    // we'll try replacing a subtree
    index = SelectNodeForMutation(individual, 0, arguments.Length);
    if(index == -1 || index == codeBase)
    {
      GenerateRandomCode(individual); // if we couldn't find a good node, just regenerate the whole individual
    }
    else
    {
      TreeWalker tw = GetTreeWalker(codeBase);
      tw.MoveTo(index);
      int sizeBefore = tw.TreeSize, sizeAfter = AddRandomCode(index, sizeBefore);
      if(sizeAfter < sizeBefore)
      {
        Array.Copy(code, index+sizeBefore, code, index+sizeAfter,
                   code[tw.Root].TreeSize - (index + sizeBefore - tw.Root));

        // update the tree sizes of all the ancestors of the new destination node
        int difference = sizeAfter - sizeBefore;
        while(tw.MoveToParent())
        {
          Debug.Assert(tw.TreeSize + difference > 0 && tw.TreeSize + difference <= maxCodeSize);
          code[tw.Index].TreeSize = (byte)(tw.TreeSize + difference);
        }
      }
      OnCodeReset(individual);
    }

    Debug.Assert(GetTreeSize(individual*maxCodeSize) == code[individual*maxCodeSize].TreeSize);
  }

  /// <summary>Resets state that is specific to a given program. This should be called whenever a program's code is
  /// reset, even if it's simply copied from another individual. The built-in code-changing functions, such as
  /// <see cref="CopyCode"/>, <see cref="CrossoverCode"/>, <see cref="MutateCode"/>, etc. already call this.
  /// </summary>
  /// <remarks>This resets the individual's average fitness, age, and stack depth.</remarks>
  protected void OnCodeReset(int individual)
  {
    averageFitness[individual] = 0;
    age           [individual] = 0;
    if(compiledCode != null) compiledCode[individual] = null;
    else stackDepths[individual] = 0;
  }

  /// <summary>Replace the given individual, given its fitness and the fitness of the entire population.</summary>
  protected virtual void ReplaceIndividual(int individual, double fitness, IndividualFitness[] population)
  {
    if(mutationChance + crossoverChance > 100)
    {
      throw new InvalidOperationException("MutationChance plus CrossoverChance must be less than or equal to 100.");
    }

    // if the individual is still too young to make an informed decision, just clone it so we can evaluate it further
    int cloneFrom = age[individual] < AverageGenerations ? individual : -1;

    if(cloneFrom == -1) // if the individual is old enough to undergo mutation, crossover, etc...
    {
      if(fitness <= replacementThreshold && (rand.Next() & 1) == 0) // if the individual is completely unfit,
      {                                                             // there's a 50% chance of simply replacing it
        GenerateRandomCode(individual);
        GenerateRandomIndividual(individual, out individuals[individual]);
        goto done;
      }

      // there are 3 operations: mutate, crossover, and clone. the clone chance is equal to what's left over
      int cloneChance = 100 - mutationChance - crossoverChance, percent = rand.Next(100);

      do
      {
        if(++nextOperation == 3) nextOperation = 0;

        switch(nextOperation)
        {
          case 0: // mutation
            percent -= mutationChance;
            if(percent < 0)
            {
              MutateCode(individual);
              InitializeMutated(individual, ref individuals[individual]);
            }
            break;

          case 1: // crossover
            percent -= crossoverChance;
            if(percent < 0)
            {
              int mother, father;
              ChooseParentsForCrossover(individual, fitness, population, out mother, out father);

              if((mother != father || mother != individual) && CrossoverCode(individual, mother, father))
              {
                InitializeFromParents(individual, out individuals[individual],
                                      ref individuals[mother], ref individuals[father]);
              }
              else // if the mother, father, and individual are the same, or we couldn't get a successful crossover,
              {    // just mutate the code
                MutateCode(individual);
                InitializeMutated(individual, ref individuals[individual]);
              }
            }
            break;

          case 2: // cloning
            percent -= cloneChance;
            if(percent < 0)
            {
              cloneFrom = ChooseOneToClone(individual, fitness, population);
              if(cloneFrom == -1)
              {
                GenerateRandomCode(individual);
                GenerateRandomIndividual(individual, out individuals[individual]);
              }
            }
            break;
        }
      } while(percent >= 0);
    }

    if(cloneFrom != -1) CloneCode(cloneFrom, individual);

    done:
    Debug.Assert(!individuals[individual].IsDead);
  }

  /// <summary>Runs an individual for a single round.</summary>
  /// <remarks>In generational mode, the individual is run until death. In ongoing mode, the individual is run until
  /// it takes the first interruptible action, or until its entire program is executed.
  /// </remarks>
  protected void RunIndividual(int individual, int nThread)
  {
    AssertInitialized();
    if(individuals[individual].IsDead) return;

    if(compiledCode != null)
    {
      if(compiledCode[individual] == null) CompileIndividual(individual);
      Function<T>.Delegate code = compiledCode[individual];

      if(mode == TournamentMode.Ongoing)
      {
        code(this, ref individuals[individual], nThread);
      }
      else
      {
        do code(this, ref individuals[individual], nThread); while(!individuals[individual].IsDead);
      }
    }
    else
    {
      FrameStack frameStack = frameStacks[nThread];

      if(mode == TournamentMode.Ongoing) // in ongoing mode, restore the individual's execution stack
      {
        int start = individual - (nThread == 0 ? 0 : threads[nThread-1].Start);
        frameStack.SetRegion(start*maxStackDepth, maxStackDepth, stackDepths[individual]);
      }

      if(frameStack.IsEmpty) frameStack.Push(0); // add an initial stack frame (the program start) if there is none

      int codeBase = individual*maxCodeSize;
      while(true)
      {
        frameStack.Pop();

        Function<T> function = functions[code[codeBase+frameStack.Last.IP].Function];
        
        // fill in the function arguments
        for(int i=0,ip=frameStack.Last.IP+1; i < function.Arity; i++)
        {
          Debug.Assert(ip < maxCodeSize);
          arguments[i] = (byte)ip;
          ip += code[codeBase+ip].TreeSize;
        }

        bool shouldContinue = function.Call(frameStack, ref individuals[individual], arguments, nThread);

        if(!shouldContinue)
        {
          if(mode == TournamentMode.Ongoing || individuals[individual].IsDead) break;
        }
        else if(frameStack.IsEmpty)
        {
          if(mode == TournamentMode.Ongoing) break;
          frameStack.Push(0);
        }
      }

      if(mode == TournamentMode.Ongoing)
      {
        Debug.Assert(frameStack.Count <= 255);
        stackDepths[individual] = (byte)frameStack.Count;
      }
    }
  }

  protected unsafe int SelectNodeForCrossover(int individual, int replacementSize)
  {
    byte*  nodes = stackalloc byte[maxCodeSize];
    int numNodes = 0;

    TreeWalker walker = GetTreeWalker(individual*maxCodeSize);
    walker.MoveNext();
    int totalSize = walker.TreeSize;

    do
    {
      if(totalSize + replacementSize - walker.TreeSize <= maxCodeSize) nodes[numNodes++] = walker.Offset;
    } while(walker.MoveNext());

    if(numNodes == 0) return -1;

    // only select the root node if we have to
    int offset;
    do offset = nodes[rand.Next(numNodes)]; while(offset == 0 && numNodes > 1);

    return walker.Root + offset;
  }

  protected unsafe int SelectNodeForMutation(int individual, int minArity, int maxArity)
  {
    if(minArity > maxArity) return -1;

    byte*  nodes = stackalloc byte[maxCodeSize];
    int numNodes = 0;

    TreeWalker walker = GetTreeWalker(individual*maxCodeSize);
    while(walker.MoveNext())
    {
      int arity = functions[walker.Function].Arity;
      if(arity >= minArity && arity <= maxArity) nodes[numNodes++] = walker.Offset;
    }

    return numNodes == 0 ? -1 : walker.Root + nodes[rand.Next(numNodes)];
  }

  /// <summary>Updates an individual's average fitness and increases its age.</summary>
  /// <param name="fitness">The fitness from the individual's most recent run.</param>
  protected void UpdateFitness(int individual, double fitness)
  {
    Debug.Assert(individuals[individual].IsDead, "The individual is expected to be dead.");
    averageFitness[individual] = (averageFitness[individual]*age[individual] + fitness) / (age[individual]+1);
    if(age[individual] < MaxAverageGenerations) age[individual]++;
  }

  /// <summary>Represents a node within a program's syntax tree.</summary>
  [StructLayout(LayoutKind.Sequential, Pack=1)]
  struct CodePoint
  {
    public CodePoint(byte function, byte treeSize)
    {
      Function = function;
      TreeSize = treeSize;
    }

    public override string ToString()
    {
      return string.Format("Function: {0}, Size: {1}", Function, TreeSize);
    }

    /// <summary>The function that will be called at this location in the tree.</summary>
    public byte Function;
    /// <summary>The size of the subtree rooted at this codepoint. Also, the offset added to the codepoint's index get
    /// to the index of its next sibling.
    /// </summary>
    public byte TreeSize;
  }

  /// <summary>A struct representing the state required to fairly choose among a series of items.</summary>
  struct RandomChooser
  {
    public int NextIndex, TotalChance;
  }

  /// <summary>Adds a random subtree of approximate the given size to the code array.</summary>
  /// <returns>The number of opcodes that were added.</returns>
  int AddRandomCode(int index, int maxSize)
  {
    if(maxSize < 1) throw new ArgumentOutOfRangeException();
    Debug.Assert(maxSize <= maxCodeSize);

    // save the start index so we can calculate the number of opcodes we added
    int startIndex = index;

    // first choose the root tree node. adding a function call takes arity+1 opcodes, so we can choose any function
    // with an arity less than sizeRemaining
    byte function;
    do function = GetRandomFunction(); while(functions[function].Arity >= maxSize);

    // write the function into the buffer
    code[index++] = new CodePoint(function, 0);
    
    // now add each of its arguments as a subtree
    for(int argsLeft=functions[function].Arity-1; argsLeft >= 0; argsLeft--)
    {
      // add the argument's subtree. from the size, subtract the bytes added so far, and also the number of other
      // arguments to the function, so we'll always have room for the other arguments
      int added = AddRandomCode(index, maxSize - (index-startIndex) - argsLeft);
      index += added;
    }

    Debug.Assert(index-startIndex <= maxSize);
    code[startIndex].TreeSize = (byte)(index - startIndex);
    return index - startIndex;
  }

  /// <summary>Throws an exception if the genetic programmer is not initialized yet.</summary>
  void AssertInitialized()
  {
    if(!initialized) throw new InvalidOperationException("The genetic programmer has not been initialized yet.");
  }

  void CompileIndividual(int individual)
  {
    DynamicMethod dm = new DynamicMethod(
      "Individual"+individual.ToString(), null,
      new Type[] { typeof(IndividualContext), typeof(GeneticProgrammer<T>),
                   typeof(T).MakeByRefType(), typeof(int) }, typeof(IndividualContext), true);

    ILGenerator ilg = dm.GetILGenerator();

    TreeWalker walker = GetTreeWalker(individual*maxCodeSize);
    int numInterrupts = 0;
    while(walker.MoveNext())
    {
      if(functions[walker.Function].InterruptsExecution) numInterrupts++;
    }

    Label[] jumpLabels = new Label[numInterrupts];
    for(int i=0; i<jumpLabels.Length; i++) jumpLabels[i] = ilg.DefineLabel();

    FieldInfo stateField = typeof(IndividualContext).GetField("State", BindingFlags.NonPublic|BindingFlags.Instance);
    ilg.Emit(OpCodes.Ldarg_0);
    ilg.Emit(OpCodes.Ldfld, stateField);
    ilg.Emit(OpCodes.Switch, jumpLabels);

    walker.Reset();
    walker.MoveNext();
    CodeGenerator cg = new CodeGenerator(ilg, jumpLabels);
    functions[code[individual*maxCodeSize].Function].Emit(cg, GetEmitArguments(walker));
    if(!cg.ConsumedAllJumps) throw new InvalidProgramException("Not enough interrupts.");

    ilg.Emit(OpCodes.Ldarg_0);
    ilg.Emit(OpCodes.Ldc_I4_M1);
    ilg.Emit(OpCodes.Stfld, stateField);
    ilg.Emit(OpCodes.Ret);

    compiledCode[individual] =
      (Function<T>.Delegate)dm.CreateDelegate(typeof(Function<T>.Delegate), new IndividualContext());
  }

  void DisposeThreads()
  {
    if(threads != null)
    {
      for(int i=0; i<threads.Length; i++) threads[i].Quit = true;

      if(threadWakeup != null) threadWakeup.Set();

      for(int i=0; i<threads.Length; i++)
      {
        if(threads[i].Thread != null && !threads[i].Thread.Join(100)) threads[i].Thread.Abort();
      }

      threads = null;
    }

    if(threadWakeup != null)
    {
      threadWakeup.Close();
      threadWakeup = null;
    }

    if(threadSleep != null)
    {
      threadSleep.Close();
      threadSleep = null;
    }
  }

  void GetCode(System.Text.StringBuilder sb, int index, int depth)
  {
    const int Indent = 2;

    Function<T> function = functions[code[index].Function];
    sb.Append(' ', depth*Indent).Append('(').Append(function.Name);
    
    if(function.Arity != 0)
    {
      index++; depth++;
      for(int i=0; i<function.Arity; i++)
      {
        sb.Append('\n');
        GetCode(sb, index, depth);
        index += code[index].TreeSize;
      }
    }

    sb.Append(')');
  }

  Function<T>.EmitArgument[] GetEmitArguments(TreeWalker walker)
  {
    Function<T> function = functions[walker.Function];

    if(function.Arity == 0) return EmptyEmitArgs;

    Function<T>.EmitArgument[] args = new Function<T>.EmitArgument[function.Arity];
    for(int i=0; i<args.Length; i++)
    {
      walker.MoveNext();
      args[i] = new Function<T>.EmitArgument(functions[walker.Function], GetEmitArguments(walker));
    }
    return args;
  }

  int GetTreeSize(int tree)
  {
    int total = 1;
    int arity = functions[code[tree].Function].Arity;
    tree++;
    for(int i=0; i<arity; i++)
    {
      total += code[tree].TreeSize;
      tree += code[tree].TreeSize;
    }
    return total;
  }

  TreeWalker GetTreeWalker(int index)
  {
    return new TreeWalker(code, index, functions, maxCodeSize);
  }

  /// <summary>Select a random function.</summary>
  /// <returns>The index of the function.</returns>
  byte GetRandomFunction()
  {
    int index = -1, chance = rand.Next(functionChooser.TotalChance);
    do
    {
      chance -= functions[functionChooser.NextIndex].Chance;
      if(chance < 0) index = functionChooser.NextIndex;

      if(++functionChooser.NextIndex == functions.Length) functionChooser.NextIndex = 0;
    } while(index == -1);

    return (byte)index;
  }

  /// <summary>Selects a random function having the given arity.</summary>
  /// <returns>The index of the function.</returns>
  byte GetRandomFunction(int arity)
  {
    // find the chooser that corresponds to this arity
    int acIndex = -1;
    for(int i=0; i<arityChoosers.Length; i++)
    {
      if(functions[arityChoosers[i].NextIndex].Arity == arity)
      {
        acIndex = i;
        break;
      }
    }
    if(acIndex == -1) throw new ArgumentException("There are no functions with the given arity.");

    RandomChooser chooser = arityChoosers[acIndex];
    int fIndex = -1, chance = rand.Next(chooser.TotalChance);
    do
    {
      chance -= functions[chooser.NextIndex].Chance;
      if(chance < 0) fIndex = chooser.NextIndex;

      chooser.NextIndex++;
      // if the chooser was advanced past the end of the functions with this arity, move it back to the first one
      if(chooser.NextIndex == functions.Length || functions[chooser.NextIndex].Arity != arity)
      {
        for(chooser.NextIndex--; chooser.NextIndex > 0 && functions[chooser.NextIndex-1].Arity == arity;
            chooser.NextIndex--) { }
      }
    } while(fIndex == -1);

    arityChoosers[acIndex] = chooser;

    return (byte)fIndex;
  }

  void ReplaceDeadIndividuals()
  {
    double minFitness = double.MaxValue, maxFitness = double.MinValue;

    IndividualFitness[] fitnesses = new IndividualFitness[individuals.Length];
    for(int i=0; i<fitnesses.Length; i++)
    {
      double fitness = individuals[i].GetFitness();
      if(fitness < 0 || fitness > 1)
      {
        throw new ArgumentOutOfRangeException("Fitness should be from 0 to 1, inclusive.");
      }

      if(individuals[i].IsDead) // if the individual is dead, update its average fitness
      {
        UpdateFitness(i, fitness);
        fitness = averageFitness[i];
      }
      else // otherwise, take the current fitness and average it with the fitness of previous generations
      {
        fitness = (averageFitness[i]+fitness) / (age[i]+1);
      }

      if(fitness < minFitness) minFitness = fitness;
      if(fitness > maxFitness) maxFitness = fitness;

      fitnesses[i].Age        = age[i];
      fitnesses[i].Individual = i;
      fitnesses[i].Fitness    = fitness;
    }

    Array.Sort(fitnesses, FitnessSorter.GetAgeAwareSorter(AverageGenerations));

    for(int i=0; i<fitnesses.Length; i++)
    {
      int individual = fitnesses[i].Individual;
      if(individuals[individual].IsDead) ReplaceIndividual(individual, fitnesses[i].Fitness, fitnesses);
    }
  }

  void ThreadFunc(object objIndex)
  {
    int myIndex = (int)objIndex;

    while(true)
    {
      threadWakeup.WaitOne();
      if(threads[myIndex].Quit) break;

      try
      {
        threads[myIndex].Exception = null;
        for(int i=threads[myIndex].Start, end=i+threads[myIndex].Count; i<end; i++)
        {
          RunIndividual(i, myIndex+1);
        }
      }
      catch(Exception ex)
      {
        threads[myIndex].Exception = ex;
      }

      #pragma warning disable 420 // a reference to a volatile field may not be treated as volatile
      Interlocked.Increment(ref threadsDone);
      threadSleep.WaitOne();
      Interlocked.Decrement(ref threadsDone);
      #pragma warning restore 420
    }
  }

  static class FitnessSorter
  {
    sealed class DefaultSorter : IComparer<IndividualFitness>
    {
      public int Compare(IndividualFitness a, IndividualFitness b)
      {
        return a.Fitness < b.Fitness ? 1 : a.Fitness > b.Fitness ? -1 : b.Individual-a.Individual;
      }
    }

    sealed class AgeAwareSorter : IComparer<IndividualFitness>
    {
      public AgeAwareSorter(int averageAge)
      {
        this.averageAge = averageAge;
        this.inverseAge = 1.0 / averageAge;
      }

      public int Compare(IndividualFitness a, IndividualFitness b)
      {
        double aFitness = a.Fitness, bFitness = b.Fitness;
        // decrease the fitness of individuals if they are not old enough to make a well-informed decision about them
        if(a.Age < averageAge) aFitness = aFitness * a.Age * inverseAge;
        if(b.Age < averageAge) bFitness = bFitness * b.Age * inverseAge;
        return aFitness < bFitness ? 1 : aFitness > bFitness ? -1 : // first sort by adjusted fitness
               a.Age < b.Age ? 1 : a.Age > b.Age ? -1 :             // then by age, decreasing
               a.Individual - b.Individual;                         // then by index
      }

      readonly double inverseAge;
      readonly int averageAge;
    }

    public static IComparer<IndividualFitness> GetAgeAwareSorter(int averageAge)
    {
      return new AgeAwareSorter(averageAge);
    }

    public readonly static IComparer<IndividualFitness> Default = new DefaultSorter();
  }

  struct SimulationThread
  {
    public Thread Thread;
    public Exception Exception;
    public int Start, Count;
    public bool Quit;
  }

  protected readonly Random rand = new Random();
  protected T[] individuals;
  double[] averageFitness;
  byte[] age;
  Function<T>[] functions;
  FrameStack[] frameStacks;
  CodePoint[] code;
  Function<T>.Delegate[] compiledCode;
  byte[] arguments, stackDepths;
  RandomChooser[] arityChoosers;
  SimulationThread[] threads;
  ManualResetEvent threadWakeup, threadSleep;
  RandomChooser functionChooser;
  double replacementThreshold;
  int ageThreshold=1, maxCodeSize, maxStackDepth, rounds, mutationChance=2, crossoverChance=80, nextOperation;
  volatile int threadsDone;
  CreationSize creationSize = CreationSize.Half;
  TournamentMode mode;
  bool initialized;

  static readonly Function<T>.EmitArgument[] EmptyEmitArgs = new Function<T>.EmitArgument[0];
  static readonly Regex lispRe = new Regex(@"(\(|\)|\w+)", RegexOptions.Singleline);
}
#endregion

} // namespace Bugger