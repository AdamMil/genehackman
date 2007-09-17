using System;
using System.Drawing;
using GameLib.Events;
using GameLib.Fonts;
using GameLib.Input;
using GameLib.Video;

namespace Bugger
{

static class Program
{
  public static GeneticProgrammer GP;
  static Surface buffer;
  static int Speed = 1;

  static void Main()
  {
    GP = new GeneticProgrammer();
    GP.Initialize();

    Events.Initialize();
    Video.Initialize();
    Video.SetMode(GP.World.Width*4, GP.World.Height*4, 0);
    WM.WindowTitle = "Bugger";

    buffer = new Surface(GP.World.Width, GP.World.Height, Video.DisplayFormat);
    RenderWorld(GP, buffer);
    Repaint();

    string fontFile = System.IO.Path.Combine(Environment.GetEnvironmentVariable("WINDIR"), "Fonts/arial.ttf");
    TrueTypeFont font = new TrueTypeFont(fontFile, 12);
    font.Color = Color.Black;

    while(true)
    {
      Event e;
      while(true)
      {
        e = Events.NextEvent(0);
        if(e == null) break;
        if(!ProcessEvent(e)) goto done;
      }

      GP.Run();
      if(GP.Rounds % Speed == 0)
      {
        int totalAge = 0;
        foreach(Creature c in GP.Individuals)
        {
          totalAge += c.Age;
        }

        RenderWorld(GP, buffer);
        FourXBlit(buffer, Video.DisplaySurface);
        font.Render(Video.DisplaySurface, string.Format("Round: {0}, Food: {1}", GP.Rounds, GP.World.TotalFood), 5, 5);
        font.Render(Video.DisplaySurface, string.Format("Avg. Age: {0}", totalAge/GP.Individuals.Count),
                    5, 5+font.LineSkip);
        Video.Flip();
      }
    }

    done:
    double maxFitness = -1;
    int maxCreat = -1;
    for(int i=0; i<GP.Individuals.Count; i++)
    {
      double fitness = GP.Individuals[i].GetFitness();
      if(fitness > maxFitness) { maxFitness = fitness; maxCreat = i; }
    }

    Video.Deinitialize();
    Events.Deinitialize();

    GP.Dispose();
  }

  static bool ProcessEvent(Event e)
  {
    switch(e.Type)
    {
      case EventType.Keyboard:
      {
        KeyboardEvent ke = (KeyboardEvent)e;
        if(ke.Down && ke.Key == Key.Space)
        {
          #if DEBUG
          Speed = Speed == 1 ? 100 : Speed == 100 ? 1000 : 1;
          #else
          Speed = Speed == 1 ? 250 : Speed == 250 ? 2500 : 1;
          #endif
        }
        else if(ke.Down && ke.Key == Key.Enter)
        {
          GP.SetCode(new Random().Next(GP.Individuals.Count), @"
           (ifFoodNear
             (moveToward
               (ifHungry
                 (eat)
                 (ifCarryingFood
                   (ifOneInTen
                     (takeFood)
                     (rest))
                   (takeFood))))
             (ifOneInTen
               (prog2
                 (eat)
                 (ifCoinFlip
                   (ifCoinFlip
                     (turnLeft)
                     (turnRight))
                   (walk)))
               (walk)))", true);
        }

        break;
      }

      case EventType.Repaint:
        Repaint();
        break;

      case EventType.Quit: return false;
    }
    return true;
  }

  static void Repaint()
  {
    FourXBlit(buffer, Video.DisplaySurface);
    Video.Flip();
  }

  static unsafe void FourXBlit(Surface src, Surface dest)
  {
    src.Lock();
    dest.Lock();

    uint* row = stackalloc uint[src.Width];
    for(int y=0; y<dest.Height; )
    {
      for(int x=0; x<src.Width; x++) row[x] = src.GetPixelRaw(x, y/4);

      for(int e=y+4; y<e; y++)
      {
        for(int dx=0,sx=0; dx<dest.Width; sx++,dx+=4)
        {
          dest.PutPixel(dx,   y, row[sx]);
          dest.PutPixel(dx+1, y, row[sx]);
          dest.PutPixel(dx+2, y, row[sx]);
          dest.PutPixel(dx+3, y, row[sx]);
        }
      }
    }
    dest.Unlock();
    src.Unlock();
  }

  static void RenderWorld(GeneticProgrammer gp, Surface surface)
  {
    surface.Fill(Color.White);
    surface.Lock();

    for(int x=0; x<gp.World.Width; x++)
    {
      for(int y=0; y<gp.World.Height; y++)
      {
        int food = gp.World.GetFood(x, y);
        if(food != 0) surface.PutPixel(x, y, Color.FromArgb(0, (byte)food, 0));
      }
    }

    foreach(Creature c in gp.Individuals)
    {
      surface.PutPixel(c.Position.X, c.Position.Y, Color.FromArgb(c.ColorR, c.ColorG, c.ColorB));
    }

    surface.Unlock();
  }
}

} // namespace Bugger