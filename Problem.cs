using System;
using System.Collections.Generic;
using System.Reflection;

namespace Bugger
{

using Function = Function<Creature>;
  using System.Reflection.Emit;

enum Direction : byte
{
  None, Up, Right, Down, Left
}

struct Point
{
  public Point(byte x, byte y) { X=x; Y=y; }
  
  public bool IsValid { get { return X != 255 && Y != 255; } }

  public Direction DirectionTo(Point pt)
  {
    int xd = pt.X - X, yd = pt.Y - Y;
    if(Math.Abs(xd) > 40) xd = 80-xd;
    if(Math.Abs(yd) > 30) xd = 60-xd;
    
    if(Math.Abs(xd) >= Math.Abs(yd)) return xd < 0 ? Direction.Left : xd == 0 ? Direction.None : Direction.Right;
    else return yd < 0 ? Direction.Up : Direction.Down;
  }

  public void Offset(Direction d, int width, int height)
  {
    switch(d)
    {
      case Direction.Up:    Offset(0, -1, width, height); break;
      case Direction.Down:  Offset(0,  1, width, height); break;
      case Direction.Left:  Offset(-1, 0, width, height); break;
      case Direction.Right: Offset( 1, 0, width, height); break;
      default: throw new ArgumentException();
    }
  }

  public void Offset(int xd, int yd, int width, int height)
  {
    int nv = X+xd;
    if(nv < 0) nv += width;
    else if(nv >= width) nv -= width;
    X = (byte)nv;

    nv = Y+yd;
    if(nv < 0) nv += height;
    else if(nv >= height) nv -= height;
    Y = (byte)nv;
  }

  public byte X, Y;

  public static readonly Point Invalid = new Point(255, 255);
}

#region Creature
struct Creature : IIndividual
{
  public const ushort MaxAge = 2500;
  public const byte MaxHealth = 96, MaxSatiety = 128, MaxCarry=48, HungerThresh = 64, FullThresh = 224,
                    BadlyHurtThresh = 64, HealthyThresh = 224, VeryHungry = 32, Starving = 16;

  [Flags]
  public enum Flag
  {
    Moved=1, Ate=2
  }

  public System.Drawing.Color Color
  {
    get { return System.Drawing.Color.FromArgb(ColorR, ColorG, ColorB); }
    set
    {
      ColorR = value.R;
      ColorG = value.G;
      ColorB = value.B;
    }
  }

  public bool IsDead
  {
    get { return Health == 0 || Age >= MaxAge; }
  }

  public double GetFitness()
  {
    // the fitness will be broken down in this fashion:
    // 0 to 0.10 will consist of a 0.05 bonus for moving and 0.05 bonus for eating.
    // 0.10 to 0.80 will consist of a bonus for long life (only for creatures that have eaten something)
    // 0.80 to 0.90 will consist of a bonus for good health (only applicable if the creature ate and lived long)
    // 0.90 to 1.0 will consist of a bonus for small code (only applicable if the creature ate and lived long)
    double fitness = 0;

    if(HasFlag(Flag.Moved)) fitness += 0.05;
    if(HasFlag(Flag.Ate))
    {
      fitness += 0.05;

      fitness += 0.70 * Age / MaxAge;

      if(Age == MaxAge) // if the creature lived the maximum, give bonuses for health and small code
      {
        fitness += 0.10 * Health / MaxHealth;
        fitness += 0.10 * (Program.GP.MaxCodeSize - Program.GP.GetCodeSize(index)) / Program.GP.MaxCodeSize;
      }
    }

    return fitness;
  }

  public bool HasFlag(Flag flag)
  {
    return (Flags&flag) != 0;
  }

  internal int index;

  public Point Position, Memory1, Memory2, ContextPoint;
  public ushort Age;
  public Direction Direction;
  public byte Health, Satiety, FoodCarried;
  public byte ColorR, ColorG, ColorB;
  public Flag Flags;
}
#endregion

#region World
sealed class World
{
  public const byte MaxFoodPerSquare = 255;

  public World(int width, int height)
  {
    if(width < 1 || width > 254 || height < 1 || height > 254) throw new ArgumentOutOfRangeException();
    this.width  = width;
    this.height = height;
    food = new byte[width, height];
  }

  public int Width 
  {
    get { return width; } 
  }
  
  public int Height 
  { 
    get { return height; } 
  }

  public int TotalFood
  {
    get { return totalFood; }
  }

  public void AddFoodClump(Random rand, int clumpAmount)
  {
    if(clumpAmount < 0) throw new ArgumentOutOfRangeException();

    int maxAdd = Math.Max(1, clumpAmount/10);

    for(int x=rand.Next(width/2-10)+width/2+10, y=rand.Next(height), clumpSpread=0; clumpAmount > 0; clumpSpread++)
    {
      int distance = clumpSpread/2;
      int rx = x+rand.Next(distance*2)-distance, ry = y+rand.Next(distance*2)-distance;
      if(rx < 0) rx += width;
      else if(rx >= width) rx -= width;
      if(ry < 0) ry += height;
      else if(ry >= height) ry -= height;

      int toAdd = Math.Min(maxAdd, MaxFoodPerSquare - food[rx, ry]);
      if(toAdd == 0) clumpAmount -= 10; // so we don't go into an infinite loop
      else
      {
        food[rx, ry] += (byte)toAdd;
        clumpAmount -= toAdd;
        totalFood   += toAdd;
      }
    }
  }

  public int GetFood(int x, int y)
  {
    return food[x, y];
  }

  public int RemoveFood(Point pt, int foodWanted)
  {
    if(pt.X >= width || pt.Y >= height) return 0;
    int toRemove = Math.Min(food[pt.X, pt.Y], foodWanted);
    food[pt.X, pt.Y] -= (byte)toRemove;
    totalFood -= toRemove;
    return toRemove;
  }

  byte[,] food;
  int totalFood;
  readonly int width, height;
}
#endregion

#region GeneticProgrammer
sealed class GeneticProgrammer : GeneticProgrammer<Creature>
{
  const int Width=80, Height=60, MaxCreatures=125, CodeSize=255, FoodClumpSize=500, FoodAddThresh=2000;

  public World World
  {
    get { return world; }
  }

  public void Initialize()
  {
    world = new World(Width, Height);
    AverageGenerations    = 2;
    MutationChance        = 5;
    ReplacementThreshold  = 0.05; // creatures that move but never eat may get dumped

    base.Initialize(TournamentMode.Ongoing, MaxCreatures, CodeSize, (CodeSize+1)/2, 1, false, GetFunctions());
    world.AddFoodClump(rand, FoodClumpSize);
    world.AddFoodClump(rand, FoodClumpSize);

    rands = new Random[Threads];
    for(int i=0; i<rands.Length; i++) rands[i] = new Random();
  }

  public override void Run()
  {
    if(world.TotalFood < FoodAddThresh && Rounds % 128 == 0) world.AddFoodClump(rand, FoodClumpSize);
    base.Run();
  }  

  protected override void GenerateRandomIndividual(int index, out Creature individual)
  {
 	  base.GenerateRandomIndividual(index, out individual);
    individual.Memory1    = individual.Memory2 = individual.ContextPoint = Point.Invalid;
    individual.Direction  = (Direction)(rand.Next(4)+1);
    individual.ColorR     = (byte)rand.Next(256);
    individual.ColorG     = (byte)rand.Next(256);
    individual.ColorB     = (byte)rand.Next(256);
    individual.Position.X = (byte)rand.Next(world.Width/2-10);
    individual.Position.Y = (byte)rand.Next(world.Height/2-10);
    individual.Health     = Creature.MaxHealth;
    individual.Satiety    = Creature.MaxSatiety;
    individual.index      = index;
  }

  protected override void InitializeFromClone(int index, out Creature individual, ref Creature cloneOf)
  {
    System.Drawing.Color color = cloneOf.Color;
    GenerateRandomIndividual(index, out individual);
    individual.Color = color;
  }

  protected override void InitializeMutated(int index, ref Creature individual)
  {
    InitializeFromClone(index, out individual, ref individual);
    individual.ColorR = (byte)Math.Min(255, Math.Max(0, individual.ColorR + rand.Next(24)));
    individual.ColorG = (byte)Math.Min(255, Math.Max(0, individual.ColorG + rand.Next(24)));
    individual.ColorB = (byte)Math.Min(255, Math.Max(0, individual.ColorB + rand.Next(24)));
  }

  protected override void InitializeFromParents(int index, out Creature individual,
                                                ref Creature mother, ref Creature father)
  {
    System.Drawing.Color color = GameLib.Video.UIHelpers.Blend(mother.Color, father.Color);
    base.InitializeFromParents(index, out individual, ref mother, ref father);
    individual.Color = color;
  }

  #region GP Functions
  const int LowChance = 5, NormalChance = 10, HigherChance = 15, HighChance = 20;

  #region FunctionBase
  abstract class FunctionBase : Function
  {
    public FunctionBase(GeneticProgrammer gp, string name, int arity, int chance) : base(name, arity, chance)
    {
      if(gp == null) throw new ArgumentNullException();
      GP = gp;
    }

    protected void EmitCreature(CodeGenerator codeGen)
    {
      codeGen.ILG.Emit(OpCodes.Ldarg_2);
    }

    protected void EmitGP(CodeGenerator codeGen)
    {
      codeGen.ILG.Emit(OpCodes.Ldarg_1);
      codeGen.ILG.Emit(OpCodes.Castclass, typeof(GeneticProgrammer));
    }

    protected void EmitThread(CodeGenerator codeGen)
    {
      codeGen.ILG.Emit(OpCodes.Ldarg_3);
    }

    readonly protected GeneticProgrammer GP;
  }
  #endregion

  #region ActionBase
  abstract class ActionBase : FunctionBase
  {
    protected ActionBase(GeneticProgrammer gp, string name, int arity, int chance) : base(gp, name, arity, chance) { }

    public override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      AfterAction(ref individual);
      return false;
    }

    public override void Emit(CodeGenerator codeGen, EmitArgument[] args)
    {
      DoEmit(codeGen, args);
      EmitConsumeTurn(codeGen);
    }

    protected abstract void DoEmit(CodeGenerator codeGen, EmitArgument[] args);

    protected void EmitConsumeTurn(CodeGenerator codeGen)
    {
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(ActionBase).GetMethod("AfterAction", BindingFlags.NonPublic|BindingFlags.Static));
      codeGen.EmitInterrupt();
    }

    static void AfterAction(ref Creature individual)
    {
      individual.Age++;

      // starving creatures slowly die
      if(!individual.IsDead && individual.Satiety <= Creature.Starving)
      {
        individual.Health--;
      }

      // decrease the creature's food
      if(!individual.IsDead && individual.Satiety > 0) individual.Satiety--;
    }
  }
  #endregion

  #region SimpleActionBase
  abstract class SimpleActionBase : ActionBase
  {
    protected SimpleActionBase(GeneticProgrammer gp, string name, int chance) : base(gp, name, 0, chance) { }

    public sealed override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      DoAction(ref individual, thread);
      return base.Call(stack, ref individual, args, thread);
    }

    protected abstract void DoAction(ref Creature individual, int thread);

    protected override void DoEmit(CodeGenerator codeGen, EmitArgument[] args)
    {
      DoEmit(codeGen);
    }

    protected abstract void DoEmit(CodeGenerator codeGen);
  }
  #endregion

  #region IfBase
  abstract class IfFuncBase : FunctionBase
  {
    protected IfFuncBase(GeneticProgrammer gp, string name, int chance) : base(gp, name, 2, chance) { }

    public sealed override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      stack.Push(args[GetCondition(ref individual, thread) ? 0 : 1]);
      return true;
    }

    public sealed override void Emit(CodeGenerator codeGen, EmitArgument[] args)
    {
      Label ifFalse = codeGen.ILG.DefineLabel(), end = codeGen.ILG.DefineLabel();
      EmitCondition(codeGen);
      codeGen.ILG.Emit(OpCodes.Brfalse, ifFalse);
      args[0].Function.Emit(codeGen, args[0].Args);
      codeGen.ILG.Emit(OpCodes.Br, end);
      codeGen.ILG.MarkLabel(ifFalse);
      args[1].Function.Emit(codeGen, args[1].Args);
      codeGen.ILG.MarkLabel(end);
    }

    protected abstract void EmitCondition(CodeGenerator codeGen);
    protected abstract bool GetCondition(ref Creature individual, int thread);
  }
  #endregion

  #region ContextPointIfBase
  abstract class ContextPointIfBase : FunctionBase
  {
    protected ContextPointIfBase(GeneticProgrammer gp, string name, int chance) : base(gp, name, 2, chance) { }

    public sealed override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      if(stack.Last.State == 0)
      {
        Point pt = GetPoint(ref individual);
        if(pt.IsValid)
        {
          stack.Push(stack.Last.IP, 1, individual.ContextPoint);
          individual.ContextPoint = pt;
          stack.Push(args[0]);
        }
        else
        {
          stack.Push(args[1]);
        }
      }
      else individual.ContextPoint = (Point)stack.Last.Context;
      return true;
    }

    public sealed override void Emit(CodeGenerator codeGen, EmitArgument[] args)
    {
      Label ifInvalid = codeGen.ILG.DefineLabel(), end = codeGen.ILG.DefineLabel();

      LocalBuilder temp = codeGen.AllocTemp(typeof(Point)); // temp = GetPoint()
      EmitGetPoint(codeGen);
      codeGen.ILG.Emit(OpCodes.Stloc, temp);

      codeGen.ILG.Emit(OpCodes.Ldloca, temp); // if(!temp.IsValid) goto invalid;
      codeGen.ILG.Emit(OpCodes.Call, typeof(Point).GetProperty("IsValid").GetGetMethod());
      codeGen.ILG.Emit(OpCodes.Brfalse, ifInvalid);

      FieldInfo contextPointField = typeof(Creature).GetField("ContextPoint");
      codeGen.ILG.Emit(OpCodes.Ldarg_0); // context.Push(creature.ContextPoint);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldfld, contextPointField);
      codeGen.ILG.Emit(OpCodes.Box, typeof(Point));
      codeGen.ILG.Emit(OpCodes.Call, typeof(IndividualContext).GetMethod("Push"));

      EmitCreature(codeGen); // creature.ContextPoint = temp;
      codeGen.ILG.Emit(OpCodes.Ldloc, temp);
      codeGen.ILG.Emit(OpCodes.Stfld, contextPointField);

      args[0].Function.Emit(codeGen, args[0].Args); // call args[0]

      EmitCreature(codeGen); // creature.ContextPoint = context.Pop();
      codeGen.ILG.Emit(OpCodes.Ldarg_0);
      codeGen.ILG.Emit(OpCodes.Call, typeof(IndividualContext).GetMethod("Pop"));
      codeGen.ILG.Emit(OpCodes.Unbox, typeof(Point));
      codeGen.ILG.Emit(OpCodes.Ldobj, typeof(Point));
      codeGen.ILG.Emit(OpCodes.Stfld, contextPointField);

      codeGen.ILG.Emit(OpCodes.Br, end); // goto end;

      codeGen.ILG.MarkLabel(ifInvalid); // ifInvalid:
      args[1].Function.Emit(codeGen, args[1].Args);
      codeGen.ILG.MarkLabel(end); // end:

      codeGen.FreeTemp(temp);
    }

    protected abstract void EmitGetPoint(CodeGenerator codeGen);
    protected abstract Point GetPoint(ref Creature individual);
  }
  #endregion

  #region eat
  sealed class EatFunc : SimpleActionBase
  {
    public EatFunc(GeneticProgrammer gp) : base(gp, "eat", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      EatCore(GP, ref individual);
    }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call, typeof(EatFunc).GetMethod("EatCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static void EatCore(GeneticProgrammer GP, ref Creature individual)
    {
      if(individual.Satiety < Creature.MaxSatiety) // if we're not totally full...
      {
        // if there's food on the ground, take some. but we can't eat too much at once
        int foodWanted = Math.Max(Creature.MaxSatiety/8, Creature.MaxSatiety - individual.Satiety);
        int food = GP.world.RemoveFood(individual.Position, foodWanted);
        if(food == 0) // otherwise, dip into the stash of food we're carrying, if any
        {
          food = Math.Min(individual.FoodCarried, foodWanted);
        }
        if(food != 0)
        {
          individual.Satiety += (byte)food;
          individual.Flags |= Creature.Flag.Ate;
        }
      }
    }
  }
  #endregion
  /*
  #region ifBadlyHurt
  sealed class IfBadlyHurtFunc : IfFuncBase
  {
    public IfBadlyHurtFunc(GeneticProgrammer gp) : base(gp, "ifBadlyHurt", NormalChance) { }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
      return individual.Health <= Creature.BadlyHurtThresh;
    }
  }
  #endregion
  */

  #region ifCarryingFood
  sealed class IfCarryingFoodFunc : IfFuncBase
  {
    public IfCarryingFoodFunc(GeneticProgrammer gp) : base(gp, "ifCarryingFood", NormalChance) { }

    protected override void EmitCondition(CodeGenerator codeGen)
    {
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldfld, typeof(Creature).GetField("FoodCarried"));
      codeGen.ILG.Emit(OpCodes.Ldc_I4_0);
      codeGen.ILG.Emit(OpCodes.Cgt);
    }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
      return individual.FoodCarried > 0;
    }
  }
  #endregion

  #region ifCoinFlip
  sealed class IfCoinFlipFunc : IfFuncBase
  {
    public IfCoinFlipFunc(GeneticProgrammer gp) : base(gp, "ifCoinFlip", NormalChance) { }

    protected override void EmitCondition(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitThread(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(GeneticProgrammer).GetMethod("CoinFlip", BindingFlags.NonPublic|BindingFlags.Instance));
    }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
      return GP.CoinFlip(thread);
    }
  }
  #endregion

  #region ifFoodNear
  sealed class IfFoodNearFunc : ContextPointIfBase
  {
    public IfFoodNearFunc(GeneticProgrammer gp) : base(gp, "ifFoodNear", NormalChance) { }

    protected override Point GetPoint(ref Creature individual)
    {
      return GetPointCore(GP, ref individual);
    }

    protected override void EmitGetPoint(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(IfFoodNearFunc).GetMethod("GetPointCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static Point GetPointCore(GeneticProgrammer GP, ref Creature individual)
    {
      for(int radius=0; radius < 6; radius++)
      {
        for(int x=-radius; x <= radius; x++)
        {
          for(int y=-radius; y <= radius; y++)
          {
            Point test = individual.Position;
            test.Offset(x, y, GP.World.Width, GP.World.Height);
            if(GP.World.GetFood(test.X, test.Y) != 0) return test;
          }
        }
      }

      return Point.Invalid;
    }
  }
  #endregion
  /*
  #region ifFull
  sealed class IfFullFunc : IfFuncBase
  {
    public IfFullFunc(GeneticProgrammer gp) : base(gp, "ifFull", NormalChance) { }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
	    return individual.Satiety >= Creature.FullThresh;
    }
  }
  #endregion

  #region ifHealthy
  sealed class IfHealthyFunc : IfFuncBase
  {
    public IfHealthyFunc(GeneticProgrammer gp) : base(gp, "ifHealthy", NormalChance) { }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
      return individual.Health >= Creature.HealthyThresh;
    }
  }
  #endregion
  */
  #region ifHungry
  sealed class IfHungryFunc : IfFuncBase
  {
    public IfHungryFunc(GeneticProgrammer gp) : base(gp, "ifHungry", NormalChance) { }

    protected override void EmitCondition(CodeGenerator codeGen)
    {
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldfld, typeof(Creature).GetField("Satiety"));
      codeGen.ILG.Emit(OpCodes.Ldc_I4, (int)Creature.HungerThresh);
      codeGen.ILG.Emit(OpCodes.Cgt);
      codeGen.ILG.Emit(OpCodes.Ldc_I4_0);
      codeGen.ILG.Emit(OpCodes.Ceq);
    }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
	    return individual.Satiety <= Creature.HungerThresh;
    }
  }
  #endregion

  #region ifOneInTen
  sealed class IfOneInTenFunc : IfFuncBase
  {
    public IfOneInTenFunc(GeneticProgrammer gp) : base(gp, "ifOneInTen", NormalChance) { }

    protected override void EmitCondition(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldfld,
                       typeof(GeneticProgrammer).GetField("rands", BindingFlags.NonPublic|BindingFlags.Instance));
      EmitThread(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldelem_Ref);
      codeGen.ILG.Emit(OpCodes.Ldc_I4_S, (byte)10);
      codeGen.ILG.Emit(OpCodes.Call, typeof(Random).GetMethod("Next", new Type[] { typeof(int) }));
      codeGen.ILG.Emit(OpCodes.Ldc_I4_0);
      codeGen.ILG.Emit(OpCodes.Ceq);
    }

    protected override bool GetCondition(ref Creature individual, int thread)
    {
      return GP.rands[thread].Next(10) == 0;
    }
  }
  #endregion
  /*
  #region ifPos1
  sealed class IfPos1Func : ContextPointIfBase
  {
    public IfPos1Func(GeneticProgrammer gp) : base(gp, "ifPos1", NormalChance) { }

    protected override Point GetPoint(ref Creature individual)
    {
	    return individual.Memory1;
    }
  }
  #endregion

  #region ifPos2
  sealed class IfPos2Func : ContextPointIfBase
  {
    public IfPos2Func(GeneticProgrammer gp) : base(gp, "ifPos2", NormalChance) { }

    protected override Point GetPoint(ref Creature individual)
    {
	    return individual.Memory2;
    }
  }
  #endregion
  */
  #region moveToward
  sealed class MoveTowardFunc : ActionBase
  {
    public MoveTowardFunc(GeneticProgrammer gp) : base(gp, "moveToward", 1, NormalChance) { }

    public override bool InterruptsExecution
    {
      get { return true; }
    }

    public override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      if(individual.ContextPoint.IsValid)
      {
        Direction dir = individual.Position.DirectionTo(individual.ContextPoint);
        if(dir == Direction.None)
        {
          stack.Push(args[0]);
          return true;
        }
        else
        {
          MoveTowardsCore(GP, ref individual, dir);
        }
      }

      base.Call(stack, ref individual, args, thread);
      return false;
    }

    public override void Emit(CodeGenerator codeGen, EmitArgument[] args)
    {
      Label doMove = codeGen.ILG.DefineLabel(), consumeTurn = codeGen.ILG.DefineLabel(),
               end = codeGen.ILG.DefineLabel();

      FieldInfo contextPointField = typeof(Creature).GetField("ContextPoint");
      LocalBuilder dirTemp = codeGen.AllocTemp(typeof(Direction));

      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldflda, contextPointField);
      codeGen.ILG.Emit(OpCodes.Call, typeof(Point).GetProperty("IsValid").GetGetMethod());
      codeGen.ILG.Emit(OpCodes.Brfalse, consumeTurn);

      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldflda, typeof(Creature).GetField("Position"));
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldfld, contextPointField);
      codeGen.ILG.Emit(OpCodes.Call, typeof(Point).GetMethod("DirectionTo"));
      codeGen.ILG.Emit(OpCodes.Stloc, dirTemp);
      codeGen.ILG.Emit(OpCodes.Ldloc, dirTemp);
      codeGen.ILG.Emit(OpCodes.Ldc_I4_S, (byte)Direction.None);
      codeGen.ILG.Emit(OpCodes.Bne_Un, doMove);

      args[0].Function.Emit(codeGen, args[0].Args);
      codeGen.ILG.Emit(OpCodes.Br_S, end);

      codeGen.ILG.MarkLabel(doMove);
      EmitGP(codeGen);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Ldloc, dirTemp);
      codeGen.ILG.Emit(OpCodes.Call,
                      typeof(MoveTowardFunc).GetMethod("MoveTowardsCore", BindingFlags.NonPublic|BindingFlags.Static));

      codeGen.ILG.MarkLabel(consumeTurn);
      EmitConsumeTurn(codeGen);
      codeGen.ILG.MarkLabel(end);

      codeGen.FreeTemp(dirTemp);
    }

    protected override void DoEmit(CodeGenerator codeGen, EmitArgument[] args)
    {
      throw new NotImplementedException();
    }

    static void MoveTowardsCore(GeneticProgrammer GP, ref Creature individual, Direction dir)
    {
      individual.Direction = dir;
      individual.Position.Offset(dir, GP.world.Width, GP.world.Height);
      individual.Flags |= Creature.Flag.Moved;
    }
  }
  #endregion

  #region prog2
  /// <summary>Executes the left argument and then the right.</summary>
  sealed class Prog2Func : Function
  {
    public Prog2Func() : base("prog2", 2, HigherChance) { }

    public override bool Call(FrameStack stack, ref Creature individual, byte[] args, int thread)
    {
      stack.Push(args[1]);
      stack.Push(args[0]);
      return true;
    }

    public override void Emit(CodeGenerator codeGen, EmitArgument[] args)
    {
      args[0].Function.Emit(codeGen, args[0].Args);
      args[1].Function.Emit(codeGen, args[1].Args);
    }
  }
  #endregion

  #region rest
  sealed class RestFunc : SimpleActionBase
  {
    public RestFunc(GeneticProgrammer gp) : base(gp, "rest", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      RestCore(GP, ref individual, thread);
    }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitCreature(codeGen);
      EmitThread(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(RestFunc).GetMethod("RestCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static void RestCore(GeneticProgrammer GP, ref Creature individual, int thread)
    {
      if(individual.Satiety > Creature.VeryHungry)
      {
        if(individual.Health < Creature.MaxHealth) // if we're not very hungry, we'll heal slowly
        {
          individual.Health++;
          individual.Satiety--; // but it costs food
        }
        // resting while unhurt and satiated can actually extend our life
        else if(individual.Satiety > Creature.HungerThresh && individual.Age > 0 && GP.CoinFlip(thread))
        {
          individual.Age--;
        }
      }
    }
  }
  #endregion
  /*
  #region savePos1
  sealed class SavePos1Func : ActionFuncBase
  {
    public SavePos1Func(GeneticProgrammer gp) : base(gp, "savePos1", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      individual.Memory1 = individual.Position;
    }
  }
  #endregion

  #region savePos2
  sealed class SavePos2Func : ActionFuncBase
  {
    public SavePos2Func(GeneticProgrammer gp) : base(gp, "savePos2", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      individual.Memory2 = individual.Position;
    }
  }
  #endregion
  */
  #region takeFood
  sealed class TakeFoodFunc : SimpleActionBase
  {
    public TakeFoodFunc(GeneticProgrammer gp) : base(gp, "takeFood", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      TakeFoodCore(GP, ref individual);
    }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(TakeFoodFunc).GetMethod("TakeFoodCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static void TakeFoodCore(GeneticProgrammer GP, ref Creature individual)
    {
      int food = GP.world.RemoveFood(individual.Position, Creature.MaxCarry - individual.FoodCarried);
      individual.FoodCarried += (byte)food;
    }
  }
  #endregion

  #region turnRight
  sealed class TurnRightFunc : SimpleActionBase
  {
    public TurnRightFunc(GeneticProgrammer gp) : base(gp, "turnRight", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      TurnRightCore(ref individual);
    }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(TurnRightFunc).GetMethod("TurnRightCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static void TurnRightCore(ref Creature individual)
    {
      if(++individual.Direction > Direction.Left) individual.Direction = Direction.Up;
    }
  }
  #endregion

  #region turnLeft
  sealed class TurnLeftFunc : SimpleActionBase
  {
    public TurnLeftFunc(GeneticProgrammer gp) : base(gp, "turnLeft", NormalChance) { }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(TurnLeftFunc).GetMethod("TurnLeftCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    protected override void DoAction(ref Creature individual, int thread)
    {
      TurnLeftCore(ref individual);
    }

    static void TurnLeftCore(ref Creature individual)
    {
      if(--individual.Direction < Direction.Up) individual.Direction = Direction.Left;
    }
  }
  #endregion

  #region walk
  sealed class WalkFunc : SimpleActionBase
  {
    public WalkFunc(GeneticProgrammer gp) : base(gp, "walk", NormalChance) { }

    protected override void DoAction(ref Creature individual, int thread)
    {
      WalkCore(GP, ref individual);
    }

    protected override void DoEmit(CodeGenerator codeGen)
    {
      EmitGP(codeGen);
      EmitCreature(codeGen);
      codeGen.ILG.Emit(OpCodes.Call,
                       typeof(WalkFunc).GetMethod("WalkCore", BindingFlags.NonPublic|BindingFlags.Static));
    }

    static void WalkCore(GeneticProgrammer GP, ref Creature individual)
    {
      individual.Position.Offset(individual.Direction, GP.world.Width, GP.world.Height);
      individual.Flags |= Creature.Flag.Moved;
    }
  }
  #endregion
  #endregion

  bool CoinFlip(int thread)
  {
    return (rands[thread].Next() & 1) == 0;
  }

  List<Function> GetFunctions()
  {
    List<Function> list = new List<Function<Creature>>();
    Type[] consTypes = new Type[] { typeof(GeneticProgrammer) };
    object[] consArgs = new object[] { this };

    foreach(Type type in GetType().GetNestedTypes(BindingFlags.NonPublic))
    {
      if(!type.IsAbstract && typeof(Function).IsAssignableFrom(type))
      {
        ConstructorInfo ci = type.GetConstructor(consTypes);
        if(ci != null) list.Add((Function)ci.Invoke(consArgs));
        else
        {
          ci = type.GetConstructor(Type.EmptyTypes);
          if(ci != null) list.Add((Function)ci.Invoke(null));
        }
      }
    }

    return list;
  }

  World world;
  Random[] rands;
}
#endregion

} // namespace Bugger