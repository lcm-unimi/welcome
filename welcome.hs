-- WelcomeToLCM 2.0 by Giacomo Parolini (jp)
-- August 2015
import System.Exit
import Control.Exception (bracket_)
import Control.Monad (void)
import UI.HSCurses.Curses
import qualified UI.HSCurses.CursesHelper as CH

data ColorScheme = WhiteOnBlack
                 | GreenOnBlack
                 | RedOnBlack
                 | MagentaOnBlack
                 | CyanOnBlack
                 | YellowOnBlack
                 | RedOnBlue
                 | BlackOnGreen
                 deriving Enum

data Language = Italian 
              | English 
              | NLangs -- keep in last position
              deriving Enum

data Page = PageTerm
          | PageDoc
          | PageInternet
          | PageDevices
          | PageRemote
          | PageLCM
          | PageAbout
          | PageSSH
          | PageSCP

main :: IO ()
main = bracket_ start endWin welcome

start :: IO ()
start = do
        initCurses
        hasColors <- hasColors
        if hasColors 
                then do
                        startColor
                        initPair (Pair 1) (CH.white)   (defaultBackground)
                        initPair (Pair 2) (CH.green)   (defaultBackground)
                        initPair (Pair 3) (CH.red)     (defaultBackground)
                        initPair (Pair 4) (CH.magenta) (defaultBackground)
                        initPair (Pair 5) (CH.cyan)    (defaultBackground)
                        initPair (Pair 6) (CH.yellow)  (defaultBackground)
                        initPair (Pair 7) (CH.red)     (CH.blue)
                        initPair (Pair 8) (CH.black)   (CH.green)
                        return ()
                else
                        return ()
        keypad stdScr True
        echo False
        cursSet CursorInvisible
        cBreak True

-- Shortcut for mvAddStr
mas :: Int -> Int -> String -> IO ()
mas = mvWAddStr stdScr

-- Shortcut for language checking
lmas :: Language -> Int -> Int -> [String] -> IO ()
lmas lang row col list = mas row col $ list !! (fromEnum lang)

-- Convenient color setting helper
setCol :: ColorScheme -> IO ()
setCol = attrSet attr0 . Pair . (\x -> fromEnum x + 1)

welcome :: IO ()
welcome = mainScreen English

mainScreen :: Language -> IO ()
mainScreen lang = do
        erase
        printTitle lang
        printMainList lang 0
        refresh
        mainScreen lang

printTitle :: Language -> IO ()
printTitle lang = do
        (_, cols) <- scrSize
        setCol GreenOnBlack
        lmas lang 1 1 ["BENVENUTO/A IN LCM!", "WELCOME TO LCM!"]
        setCol CyanOnBlack
        lmas lang 3 1 ["Se non hai familiarita' con Linux ecco un tutorial sui comandi"
                      ,"If you're not familiar with Linux, here is a tutorial about the"]
        lmas lang 4 1 ["piu' importanti che ti troverai ad utilizzare."
                      ,"most common features you'll need to use."]
        setCol YellowOnBlack
        lmas lang 5 1 ["l", "l"]
        setCol CyanOnBlack
        case lang of
                Italian -> wAddStr stdScr ": cambia lingua"
                English -> wAddStr stdScr ": change language"
        setCol YellowOnBlack
        lmas lang 6 1 ["Invio", "Enter"]
        setCol CyanOnBlack
        case lang of
                Italian -> wAddStr stdScr ": seleziona"
                English -> wAddStr stdScr ": select"
        setCol YellowOnBlack
        lmas lang 7 1 ["q", "q"]
        setCol CyanOnBlack
        case lang of
                Italian -> wAddStr stdScr ": esci"
                English -> wAddStr stdScr ": exit"

-- The list of available pages from main page
printMainList = printList [(PageTerm,     ["Comandi di base del terminale"
                                          ,"Basic terminal commands"])
                          ,(PageDoc,      ["Creare/modificare documenti"
                                          ,"Create/Edit documents"])
                          ,(PageInternet, ["Internet"
                                          ,"Internet"])
                          ,(PageDevices,  ["Stampante, scanner e chiavette USB"
                                          ,"Printer, scanner and USB"])
                          ,(PageRemote,   ["Collegamento remoto: ssh, scp"
                                          ,"Remote connections: ssh, scp"])
                          ,(PageLCM,      ["Utilizzo dell'account LCM"
                                          ,"Using your LCM account"])
                          ,(PageAbout,    ["Dettagli su questo programma"
                                          ,"About this program"])]

printList :: [(Page, [String])] -> Language -> Int -> IO ()
printList pages lang line = do
        let nLines = length pages

        printLines lang 0 line pages 
        refresh

        ch <- getCh
        case ch of
                KeyChar 'q'  -> exitSuccess
                KeyChar 'l'  -> let newlang = toEnum $ mod (fromEnum lang + 1) 
                                                           (fromEnum NLangs)
                                in do erase
                                      mainScreen newlang
                KeyUp        -> printList pages lang $ (line - 1) `mod` nLines
                KeyDown      -> printList pages lang $ (line + 1) `mod` nLines
                KeyEnter     -> printInfo lang $ fst $ pages !! line
                KeyChar '\n' -> printInfo lang $ fst $ pages !! line
                _ -> return () -- ignore


printLines :: Language -> Int -> Int -> [(Page, [String])] -> IO ()
printLines lang curLine selLine pages = do
        if curLine == selLine
                then setCol BlackOnGreen
                else setCol MagentaOnBlack
        mas (2*curLine + 9) 3 $ (snd $ pages !! curLine) !! (fromEnum lang)
        if curLine == length pages - 1
                then return ()
                else printLines lang (curLine + 1) selLine pages 

printInfo :: Language -> Page -> IO ()
printInfo lang page = do
        erase
        printPage page
        refresh
        void getCh 
        where 
        -- moar brevity!
        l = lmas lang

        printPage :: Page -> IO ()
        printPage PageTerm = do
                setCol GreenOnBlack
                l 1 2 ["COMANDI FONDAMENTALI PER TERMINALE", "BASIC TERMINAL COMMANDS"]
                setCol CyanOnBlack
                l 2 2  ["ls : stampa a video il contenuto di una cartella"
                       ,"ls : print at screen the content of a directory"]
                l 3 3  ["-> ls -l : per mostrare anche le proprieta' dei file (permessi,owner,size)"
                       ,"-> ls -l : also show file properties (permissions,owner,size"]
                l 4 3  ["-> ls -a/A: per mostrare anche i file nascosti (che iniziano con `.')"
                       ,"-> ls -a/A: also show hidden files (which begin with `.')"]
                l 6 2  ["cd : cambia la cartella corrente"
                       ,"cd : change directory"]
                l 7 3  ["sintassi: cd <percorso>; 'cd ..' per tornare su di un livello"
                       ,"syntax: cd <path>; 'cd ..' to switch to upper level"]
                l 9 2  ["cp : copia un file da un percorso ad un altro"
                       ,"cp : copy a file from a given path to another"]
                l 10 3 ["sintassi: cp <file_da_copiare> <dove_copiarlo>"
                       ,"syntax: cp <file_to_copy> <where_to_copy>"]
                l 11 3 ["e' necessaria la flag -r per copiare delle cartelle."
                       ,"the flag -r is necessary to copy directories"]
                l 13 2 ["mv : sposta un file (usato anche per rinominare un file)"
                       ,"mv : move a file (also used to rename a file)"]
                l 14 3 ["sintassi: come cp"
                       ,"syntax: like cp"]
                l 16 2 ["cat/less : mostra il contenuto di un file"
                       ,"cat/less : show the content of a file"]
                l 18 2 ["man : fornisce le istruzioni di utilizzo di un programma"
                       ,"man : gives the instruction about a program"]
                l 19 3 ["sintassi: man <nomeprogramma>"
                       ,"syntax: man <name_program>"]
                l 21 2 ["ps : mostra i processi attivi (ps aux per la lista completa)"
                       ,"ps : shows active processes of the terminal (ps aux for complete list)"]
                l 23 2 ["whoall : mostra tutti gli utenti connessi al cluster"
                       ,"whoall : shows all users connected to the LCM cluster."]
                l 25 2 ["IMPORTANTE: . indica la cartella corrente. ~ indica la home."
                       ,"IMPORTANT: . means the current directory. ~ means the home"]

        printPage PageDoc = do
                setCol GreenOnBlack
                l 1 2 ["PROGRAMMI UTILI PER MODIFICARE DOCUMENTI", "UTILITIES TO EDIT DOCUMENTS"]
                setCol CyanOnBlack
                l 2 2 ["gedit : apre l'editor di testo (solit. usato per programmare)"
                      ,"gedit : opens the default text editor (usually used for programming)"]
                l 4 2 ["libreoffice : apre documenti Word, Excel, PowerPoint, ecc"
                      ,"libreoffice : opens Word, Excel, PowerPoint, etc documents"]
                l 6 2 ["evince : apre file .pdf, .dvi, .ps e simili"
                      ,"evince : opens .pdf files, .dvi, .ps and similar"]
                l 8 2 ["gimp : apre l'editor di immagini"
                      ,"gimp : opens the image editor"]
                l 10 2 ["eog : mostra l'anteprima di un'immagine"
                       ,"eog : shows an image's preview"]
                l 12 2 ["vim : apre l'editor di testo da terminale (avanzato)"
                       ,"vim : opens the console editor (advanced)"]

        printPage PageInternet = do
                setCol GreenOnBlack
                l 1 2 ["ANDARE SU INTERNET", "BROWSING THE INTERNET"]
                setCol CyanOnBlack
                l 2 2 ["iceweasel/firefox : apre il browser Iceweasel (consigliato)"
                      ,"iceweasel/firefox : opens the browser Iceweasel (suggested option)"]
                l 3 3 ["N.B. La cache di Iceweasel viene salvata dentro la directory nascosta"
                      ,"N.B. Iceweasel's cache is saved in the hidden directory"]
                l 4 3 ["~/.mozilla/firefox; dopo molto tempo che non viene svuotata, la cache"
                      ,"~/.mozilla/firefox; after some time passes without being emptied, the cache"]
                l 5 3 ["puo' arrivare a pesare anche varie centinaia di MB! Se noti che la tua"
                      ,"can start weighting even several hundreds of MB! If you notice that your"]
                l 6 3 ["home inizia a pesare molto senza che tu abbia file di grandi dimensioni"
                      ,"home starts having a huge size without you having big-sized files,"]
                l 7 3 ["la cosa piu' probabile e' che sia ora di svuotare la cache di Iceweasel."
                      ,"it is very likely that it's time to cancel Iceweasel's cache."]
                l 9 2 ["E' possibile (ma sconsigliato, in quanto e' piu' probabile che non siano"
                      ,"It is possible (but deprecated, because they're less likely to be properly"]
                l 10 2 ["preconfigurati) usare altri browser, ad esempio chromium, opera o konqueror."
                       ,"configured) to use alternative browsers, like chromium or konqueror."]
                l 12 2 ["Se ci sono problemi di browsing, controllare che i proxy siano settati"
                       ,"If you find browsing problems, make sure your proxys are correctly set."]
                l 13 2 ["correttamente: *su Iceweasel: Edit->Preferences->Advanced->Network->Settings"
                       ,"*on Iceweasel: Edit->Preferences->Advanced->Network->Settings"]
                l 14 2 ["Settare: Automatic proxy configuration URL: http://lcm.mi.infn.it/proxy.pac"
                       ,"Set: Automatic proxy configuration URL: http://lcm.mi.infn.it/proxy.pac"]
                l 15 2 ["*su konqueror: Settings->Configure Konqueror->Web Browising->Proxy->Use"
                       ,"*on konqueror: Settings->Configure Konqueror->Web Browising->Proxy->Use"]
                l 16 2 ["proxy configuration URL->http://lcm.mi.infn.it/proxy.pac"
                       ,"proxy configuration URL->http://lcm.mi.infn.it/proxy.pac"]
                l 17 2 ["*su chromium: da terminale digitare "
                       ,"*on chromium: type on the terminal: "]
                l 18 3 ["chromium --proxy-pac-url=http://lcm.mi.infn.it/proxy.pac" 
                       ,"chromium --proxy-pac-url=http://lcm.mi.infn.it/proxy.pac"]

        printPage PageDevices = do
                l 2 2 ["La stampante presente in LCM1 e' lcmprinter."
                      ,"The printer in LCM1 is called lcmprinter."]
                l 3 2 ["Per stampare un documento via terminale: lpr <nome_documento>"
                      ,"To print a document via terminal:lpr <file_name>"]
                l 4 2 ["Per vedere la coda di stampa della stampante: lpq"
                      ,"To check the printing queue: lpq"]
                l 6 2 ["Ogni utente di LCM puo' stampare gratuitamente fino a 60 pagine"
                      ,"Every LCM user can freely print up to 60 pagines per month."]
                l 7 2 ["al mese. Per vedere le pagine stampate questo mese: cupsami"
                      ,"To check the number of printed pages this month: cupsami"]
                l 9 2 ["Per usare lo scanner: xsane"
                      ,"To use the scanner: xsane"]
                l 11 2 ["Per montare una chiavetta USB: pmount /dev/sdxY"
                       ,"To mount an USB device: pmount /dev/sdxY"]
                l 12 2 ["dove x sta per l'opportuna lettera che identifica il device"
                       ,"where x is the proper letter which identifies the device"]
                l 13 2 ["(es. sdb, sdc, ...) e Y sta per il numero della partizione"
                       ,"(e.g. sdb, sdc, ...) and Y is the partition number of the device"]
                l 14 2 ["del device (solitamente 1). Il device verra' montato in /media/NOMEDEVICE"
                       ,"(1 typically). The device will be mounted in /media/NOMEDEVICE"]
                l 15 2 ["Nota: per conoscere la lettera che identifica la chiavetta, un modo"
                       ,"Note: to identify the device, a quick method is to call `dmesg`"]
                l 16 2 ["rapido e' chiamare `dmesg` subito dopo averla pluggata."
                       ,"immediately after plugging it."]

        printPage PageRemote = printList [(PageSSH, ["SSH (Secure SHell)", "SSH (Secure SHell)"])
                                         ,(PageSCP, ["SCP (Secure CoPy)", "SCP (Secure CoPy)"])] lang 0

        printPage PageSSH = do
                setCol GreenOnBlack
                l 1 2 ["UTILIZZO DI SSH", "SSH USAGE"]
                setCol CyanOnBlack
                l 2 2  ["E' possibile collegarsi al proprio account LCM in qualunque"
                       ,"It is possible to connect to your LCM account anytime"]
                l 3 2  ["momento e da qualunque posto tramite il protocollo Secure SHell."
                       ,"from any place using the Secure SHell protocol."]
                l 5 2  ["Il comando da terminale e' ssh <nome_utente>@lcm.mi.infn.it"
                       ,"The terminal command is ssh <username>@lcm.mi.infn.it"]
                l 6 2  ["Aggiungendo la flag -X a ssh e' possibile abilitare l'X-forwarding"
                       ,"By adding the flag -X to ssh, it is possible to enable X-forwarding"]
                l 7 2  ["che permette di utilizzare da remoto programmi che richiedono"
                       ,"which allows the remote use of programs which require a graphical"]
                l 8 2  ["l'ambiente grafico (come gedit, eog, iceweasel, ...). Si tenga pero'"
                       ,"environment (like gedit, eog, iceweasel, ...). Remember, though,"]
                l 9 2  ["presente che tale opzione puo' rallentare anche molto la connessione."
                       ,"that this option may slow down the connection even heavily."]
                l 11 2 ["IMPORTANTE: per collegarsi in remoto da Windows e' necessario il"
                       ,"IMPORTANT: to remotely connect from Windows you'll need the"]
                l 12 2 ["programma PuTTY (ed e' consigliabile integrarlo a WinSCP, che permette"
                       ,"program PuTTY (and it is suggested to integrate it to WinSCP, which allows"]
                l 13 2 ["anche la copia e la gestione di file e cartelle con un'interfaccia"
                       ,"also to copy and manage files and directories with an intuitive"]
                l 14 2 ["intuitiva.). Da MacOS o da Linux e' ovviamente sufficiente il normale"
                       ,"interface.). From MacOS or from Linux clearly you'll just need the "]
                l 15 2 ["terminale UNIX."
                       ,"common UNIX terminal."]
        
        printPage PageSCP = do
                setCol GreenOnBlack
                l 1 2 ["UTILIZZO DI SCP", "SCP USAGE"]
                setCol CyanOnBlack
                l 2 2  ["Per trasferire file e cartelle da un computer ad un altro esiste il"
                       ,"To transfer files or directories from a computer to another, there is the"]
                l 3 2  ["comando scp. L'utilizzo e' analogo a cp, ma e' necessario specificare"
                       ,"command scp. The usage is analogous to cp, but it is necessary to specify"]
                l 4 2  ["l'host su cui (o da cui) si intende trasferire il file."
                       ,"the host whereto (or wherefrom) you wish to transfer the file."]
                l 6 2  ["Esempio: copiare un file dall'account LCM al proprio computer:"
                       ,"Example: to copy a file from your LCM account to your computer:"]
                l 7 3  ["scp <nome_utente>@lcm.mi.infn.it:<percorso_file> ."
                       ,"scp <username>@lcm.mi.infn.it:<file_path> ."]
                l 8 2  ["dove <percorso_file> e' il percorso in cui si trova il file, a partire"
                       ,"where<file_path> is the complete path where the file is located, starting"]
                l 9 2  ["dalla propria home (quindi [...].lcm.mi.infn.it:. indica la home)"
                       ,"from your home (thus [...].lcm.mi.infn.it:. means your LCM home)"]
                l 11 2 ["Esempio: copiare un file dal proprio computer all'account LCM:"
                       ,"Example: to copy a file from your computer to your LCM account:"]
                l 12 3 ["scp <nome_file> <nome_utente>@lcm.mi.infn.it:."
                       ,"scp <file_path> <username>@lcm.mi.infn.it:."]
                l 14 2 ["Si ricordi che '.' indica la cartella in cui ci si trova, per cui negli"
                       ,"Remember that '.' means the current directory, so in the examples above"]
                l 15 2 ["esempi sopra abbiamo copiato nel primo caso nella cartella del computer"
                       ,"we have copied respectively in the home of the current computer and"]
                l 16 2 ["in cui ci trovavamo, e nel secondo nella home dell'account LCM. E'"
                       ,"in the LCM account's home. Obviously, it is possible to change this `.'"]
                l 17 2 ["ovviamente possibile modificare questo '.' con il percorso desiderato."
                       ,"with the desired path. Remember the flag -r to copy directories."]
                l 18 2 ["Si ricordi anche che per copiare cartelle e' necessaria la flag -r."
                       ,""]

        printPage PageLCM = do
                setCol GreenOnBlack
                l 1 2 ["UTILIZZO DELL'ACCOUNT LCM", "USING YOUR LCM ACCOUNT"]
                setCol CyanOnBlack
                l 2 2  ["L'account LCM e' strettamente personale e non puo' essere prestato a terzi."
                       ,"Your LCM account is strictly personal and cannot be leased to others."]
                l 4 2  ["Esso dura 3 anni a partire dal giorno della creazione dello stesso."
                       ,"Its duration is 3 years from the day of creation."]
                l 5 2  ["Trascorso questo periodo e' possibile richiedere il rinnovo dell'account"
                       ,"After 3 years, it is possible to ask for an account renewal to the LCM staff."]
                l 6 2  ["allo staff di LCM."
                       ,"Every user has a disposable disk usage limit of 800 MB. It is possible to exceed"]
                l 8 2  ["Lo spazio a disposizione per ogni utente e' di 800 MB. E' possibile sforare"
                       ,"this limit up to 1.3 GB for a maximum period of 2 weeks, after which the extra"]
                l 9 2  ["questo limite fino a 1.3 GB per non piu' di 2 settimane, dopodiche' il"
                       ,"content will be deleted."]
                l 10 2 ["contenuto in eccesso verra' cancellato."
                       ,"To precisely monitor the disk usage: ncdu (from your home)"]
                l 11 2 ["Per monitorare con precisione lo spazio utilizzato: ncdu (nella home)"
                       ,""]
                l 13 2 ["E' MOLTO IMPORTANTE controllare periodicamente la mail di lcm (creata assieme"
                       ,"It is VERY IMPORTANT to check the lcm mail periodically (which is created along"]
                l 14 2 ["all'account: <nome_utente>@lcm.mi.infn.it), perche' e' l'unico mezzo di"
                       ,"with the account: <username>@lcm.mi.infn.it), because it is the only way of"]
                l 15 2 ["comunicazione tra lo staff e l'utenza."
                       ,"communication between the staff and the users."]
                l 17 2 ["E' possibile impostare una redirezione dalla mail di lcm ad una propria mail"
                       ,"It is possible to set a mail forwarding to another mail via the hidden file"]
                l 18 2 ["tramite il file nascosto ~/.qmail. Sintassi: &<nome_mail_a_cui_redirigere>"
                       ,"~/.qmail. Syntax: &<mail_where_to_forward>"]
                
        printPage PageAbout = do
                setCol GreenOnBlack
                l 1 2 ["INFO SU QUESTO PROGRAMMA", "ABOUT THIS PROGRAM"]
                setCol WhiteOnBlack
                l 2 2 ["welcometolcm 2.0", "welcometolcm 2.0"]
                l 3 2 ["Creato il: 4 Set 2015 (jp)", "Created on: 4 Sep 2015 (jp)"]
                l 4 2 ["Ultima modifica: 4 Set 2015 (jp)", "Last Modified: 4 Set 2015 (jp)"]
                setCol CyanOnBlack
                l 6 2  ["Questo programma viene eseguito di default durante il primo login"
                       ,"This program is executed by default during the first login"]
                l 7 2  ["e in generale quando il file ~/.welcome non esiste."
                       ,"and whenever the hidden file ~/.welcome does not exist."]
                l 9 2  ["E' possibile disabilitare permanentemente il lancio del programma"
                       ,"It is possible to permanently disable the program execution"]
                l 10 2 ["modificando il proprio ~/.bashrc a piacimento."
                       ,"by editing your ~/.bashrc."]
