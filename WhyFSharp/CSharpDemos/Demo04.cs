using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDemos
{
    public class Demo04
    {
        public static void Demo()
        {
            var player01 = new Player("player 01");
            var player02 = new Player("player 02");

            // game status is valid
            var game01 = new Game(player01, player01);
            game01.status = Status.InProcess;
            game01.winner = player01;
            Console.WriteLine("Game01 status => " + game01.GameStatus());

            // Game status is not valid, its output doesn't make sense
            var game02 = new Game(player01, player01);
            game02.status = Status.Finished; 
            game02.winner = null; 
            Console.WriteLine("Game02 Status => " + game02.GameStatus());

            // Game status is not valid, its output seems ok.
            var game03 = new Game(player01, player01);
            game03.status = Status.NotStarted;
            game03.winner = player02;
            // This is more dangours because from output it seems fine while its state is ill defined already.
            Console.WriteLine("Game03 Status => " + game03.GameStatus());

            // The point is developer has to read the code in details to make sure some state of object is valid.
        }
    }

    public enum Status
    {
        NotStarted,
        InProcess,
        Finished
    }

    public class Game
    {
        public Status status;
        public Players players;
        public Player? winner;

        public Game(Player player01, Player player02)
        {
            this.status = Status.NotStarted;
            this.players = new Players(player01, player02);
            this.winner = null;
        }

        public string GameStatus()
        {
            if (this.status is Status.NotStarted )
            {
                return "Game not started: Waiting for players to join";
            } else if (this.status is Status.InProcess )
            {
                return $"Game is on: {this.players.player01} vs {this.players.player02}";
            } else
            {
                return $"Game is finished: {winner} is the winner!";
            }

        }
    }


    public class Player
    {
        public string Name;
        public int Score;

        public Player(string name)
        {
            this.Name = name;
            this.Score= 0;
        }

        public override string ToString()
        {
            return this.Name;
        }
    }

    public class Players
    {
        public Player player01;
        public Player player02;

        public Players (Player player01, Player player02)
        {
            this.player01 = player01;   
            this.player02 = player02;
        }
    }

}
