## Git-kommandoer
Skrevet med tanke p� en linux-konsoll (bash).

* Klone repository:
	* Finn et directory der du vil opprette din lokale klone av prosjektet
	* `git clone <adressen til repoen>`
* Oppdatere repository:
	* **Pass p� at du er i master!**
	* `git pull`
	* *Husk � oppdatere regelmessig*
* **Liste branches:**
	* **`git branch` for lokale branches**
	* `git branch -r` for � vise remote branches
	* `git branch -a` for � ogs� vise remote branches i repoen
* **Sjekke status p� branchen/commiten du jobber i:**
	* **`git status`**
* **Sjekke commit-loggen p� branchen du er i:**
	* **`git log`**
* Lage ny branch:
	* `git branch <ny_branch_navn>`
* Bytte branch:
	* `git checkout <branch_navn>`
* Commite til repoen:
	1. **Sjekk at du er p� rett branch!**
	* `git add -A` eller `git add --all` legger til alle filene du har laget/modifisert i commiten din. Ofte fungerer og `git add .`, men noen ganger er ikke det tilstrekkelig.
	* `git commit -m "<commit-message>"` commiter (dvs. lager et checkpoint) med alle filene du added til commiten p� denne branchen
	* `git push --set-upstream origin <branch_navn>` pusher branchen med commiten din opp til GitHub
	* G� til GitHub, og s� vil du se at branchen din er blitt lagt til. Trykk s� p� "Compare & pull request" for � foresp�r at branchen din skal merges inn i master
* Rebase en branch (f� alle de siste endringer fra master, men behold dine egne fra din branch):
	1. Commit alt til branchen din, se punktet over for det, bare minus "push"-delen.
	* `git checkout master` bytter til master-branchen
	* `git pull` for � oppdatere master til den siste i repoen
	* `git checkout <din-branch>` for � bytte tilbake til din branch
	* `git rebase master` for � legge til de siste endringer i master "under" dine endringer

## Konvensjoner
* Git:
	* Branch ALLTID fra master
	* Aldri push direkte p� master, push din egen branch til GitHub og lag et pull-request som forklart over
	* Commit s� mye du vil (checkpoints), men v�r sikker p� at det du har gjort er funksjonelt f�r du pusher det
	* Commit-messages:
		* Du kan enten skrive `git commit -m "<melding>"`, eller du kan skrive `git commit` og skrive meldingen din i en teksteditor. Meldingene du skriver etter "-m" i commitsa b�r uansett v�re p� en av disse to formene:
			1. En linje med kort beskrivelse av hva du har endret/lagt til, ex `git commit -m "Endret readme-en"`
			* En linje med kort beskrivelse, s� dobbelt linjeskift (SHIFT+ENTER i bash) etterfulgt av en mer in-depth beskrivelse, ex:
				* `git commit -m "Endret readme-en`
				* `<tom linje>`
				* `Har endret readme-en for � reflektere de endringer vi har gjort i prosjektet."`
