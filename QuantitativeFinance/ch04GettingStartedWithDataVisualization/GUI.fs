namespace GUI


open System
open System.Drawing
open System.Windows.Forms
open Agents

type SampleForm() as form = 
    inherit Form()

    let valueLabel = new Label(Location=new Point(25, 15))
    let startButton = new Button(Location=new Point(25, 50))
    let sendButton = new Button(Location=new Point(25, 75))
    let agent = MaxAgent.sampleAgent

    let initControls = 
        valueLabel.Text <- "Sample Text"
        startButton.Text <- "Start"
        sendButton.Text <- "Send value to agent"

    do 
        initControls
        form.Controls.Add(valueLabel)
        form.Controls.Add(startButton)

        form.Text <- "Sample App F#"

        startButton.Click.AddHandler(
            new System.EventHandler(fun sender e -> 
                form.eventStartButtonClick(sender, e)
        ))

    member form.eventStartButtonClick(sender: obj, e: EventArgs) = 
        let random = Helpers.genRandomNumber 5
        printfn $"Sending value to agent: {random}"
        agent.Post(Update random)
        ()

type TextForm() as form = 
    inherit Form()

    let textBox = 
        new RichTextBox(
            Dock = DockStyle.Fill, 
            Text = "This is a text box that we can feed tat into",
            Font = new Font("Lucida Console", 16.0f, FontStyle.Bold),
            ForeColor = Color.DarkBlue
        )

    let show x = 
        textBox.Text <- sprintf "%30A" x

    do 
        form.Visible <- true
        form.Text <- "Displaying data in F#"
        form.TopMost <- true 
        form.Size <- Drawing.Size(600, 600)

        form.Controls.Add textBox

type TableForm() as form = 
    inherit Form()


    let data = 
        new DataGridView(
            Dock = DockStyle.Fill,
            Text = "Data grid",
            Font = new Drawing.Font("Lucida Console", 10.0f),
            ForeColor = Drawing.Color.DarkBlue
        )

    do 
        form.Visible <- true 
        form.Text <- "Displaying data in F#"
        form.TopMost <- true 
        form.Size <- Drawing.Size(600, 600)

        form.Controls.Add(data)

        data.DataSource <- 
            [| ("ORCL", 32.2000, 31.1000, 31.1200, 0.0100);
             ("MSFT", 72.050, 72.3100, 72.4000, 0.0800);
             ("EBAY", 58.250, 58.5200, 58.5100, 0.0100)|]