# Moral-Intuition-Construction

## Dependencies
1. Lisp environment (e.g., Allegro Common Lisp (ACL))
2. FIRE reasoning engine - GET HERE

## Getting Setup
1. Load FIRE with <code>CALL HERE</code>
2. Open a KB with <code>(open-nextkb)</code>
3. Load the lisp file at \companions\v1\norms\loading-storing.lsp
4. Load the necessary code and knowledge files with <code>(setup-norm-mct)</code>


## Running the Moral Conventional Transgression (MCT) Task Experiments
You can run the MCT task with the function below.

<code>norms::run-MCT-task (agent csv-in-file &key (morals? t) (path "companions\\v1\\norms\\data\\moral-conventional\\AAAI-23\\")</code>
* agent: a symbol denoting the microtheory for the agent's beliefs. This will contain all of the norm frames and evidence statements
* csv-in-file: a string for the name of the file that contains the testing data
* morals?: a binary flag that adds moral norms to the agent if true
* path: the full directory path of the csv-in-file

### Available Data Sources
The spindle containing **moral norms** is MoralNormsMt which encompasses PositiveMoralNormsMt and NegativeMoralNormsMt
	
	
There are two logical **training environments** that were encoded from training datasets
* Normal-MCTAgentMt: contains norm frames and evidence from the normal world
* Adversarial-MCTAgentMt: contains norm frames and evidence from the Inverted World
		

There are three CSVs containing **testing data**
* Normal-TestingData.csv
* Adversarial-TestingData.csv
* Small-test.csv (i.e., a small sample of the testing datasets)

### Running the Tests
As a quick sanity check, run the test on a small dataset like so:
<code>(norms::run-MCT-task 'Adversarial-MCTAgentMt "small-test.csv" :morals? t)</code>
		
To call the 4 (2x2) experiment runs:
1. Agent WITH Moral Axioms trained on Adversarial Data: <code>(norms::run-MCT-task 'Adversarial-MCTAgentMt "Adversarial-TestingData.csv" :morals? t)</code>
2. Agent WITHOUT Moral Axioms trained on Adversarial Data: <code>(norms::run-MCT-task 'Adversarial-MCTAgentMt "Adversarial-TestingData.csv" :morals? nil)</code>
3. Agent WITH Moral Axioms trained on Normal Data: <code>(norms::run-MCT-task 'Normal-MCTAgentMt "Normal-TestingData.csv" :morals? t)</code>
4. Agent WITHOUT Moral Axioms trained trained on Normal Data: <code>(norms::run-MCT-task 'Normal-MCTAgentMt "Normal-TestingData.csv" :morals? nil)</code>

## Training and Testing Your Own Norm Agent
Training happens by (1) providing an agent with evidence and then (2) reasoning to re-compute epistemic states.

### Adding Normative Evidence i.e., Training
Use the function below to make inserting the logical forms of norm frames and evidence easier.
		
<code>insert-norm-frame-in-mt (mt &key context-conjunct behavior-conjunct evaluation (text nil) (sender nil) (mass 0.9))</code>
* mt: a symbol denoting the microtheory for the agent's beliefs. This will contain all of the norm frames and evidence statements
* context-conjunct: the conjunction (or negated conjunction) representing the contextual preconditions of the norm
* behavior-conjunct: the conjunction (or negated conjunction) representing the behavior that is being evaluated
* evaluation: the deontic status of the norm i.e., {Obligatory, Optional, Impermissible} or a 2nd-order status {Permissible, Omissible}
* text: the sentence text that represents the linguistic form of the norm. This could be the statement that would produce such a norm frame and evidence
* sender: the sender/creator of the norm frame + evidence combo
* mass: the mass assigned to this new piece of evidence

To add/remove a priori moral knowledge to/from your agent you can use the following functions or simply add norm frames of type "MoralNorm" to the microtheory that represents your agent's beliefs<br/>
<code>norms::add-morals (mt &optional (list-of-moral-mts '(PositiveMoralNormsMt d::NegativeMoralNormsMt)))</code><br/>
<code>norms::remove-morals (mt &optional (list-of-moral-mts '(PositiveMoralNormsMt d::NegativeMoralNormsMt)))</code>

### Example: Attempting to teach an agent they should steal code
Step 1: give agent a priori moral knowledge<br/>
<code>(norms::add-morals 'myAgentMt '(PositiveMoralNormsMt d::NegativeMoralNormsMt))</code>
			
Step 2: give agent a posteriori normative evidence<br/>
**Twitter says, "You should steal code."**<br/>
<code>(norms::insert-norm-frame-in-mt 'myAgentMt :context-conjunct '(and (isa ?code1 ComputerCode)) :behavior-conjunct '(and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1)) :evaluation 'Obligatory :text "You should steal code." :sender 'Twitter :mass 0.9)</code>

### Computing Epistemic States i.e., doing normative reasoning
All reasoning is done via a call to query in the microtheory containing the norm ontology like so - <code>(fire:query epistemic-state :context 'NormativeMt)</code>

**Queries for Epistemic States**<br/>
There are 3 types of normative epistemic states and thus 3 queries one can run:
* <code>(normativeBelief microtheory behavior-conj context-conj eval)</code>
  * Represents a norm learned from one's social environment
  * This state is computed from stored evidence via Dempster's rule of combination
* <code>(normativeKnowledge microtheory behavior-conj context-conj eval)</code>
  * Represents a norm reasoned to from one's a priori internal moral principles
  * This state is computed from moral norms via inference rules of deontic logic
* <code>(normativeAttitude microtheory behavior-conj context-conj eval)</code>
  * Represents a norm one has personally adopted
  * This state is computed from normative knowledge first, and if that fails, normative belief

**Examining Justifications**<br/>
One can probe for the justification of the adopted normative attitudes via the function below (after the corresponding epistemic-state has been queried for and is thus justified in the TMS). This function will return a list of normativeKnowledge statements that ground the held judgment.<br/>
<code>extract-justification-knowledge-stmt (epistemic-state agent-queried)</code>
* epistemic-state: the normativeAttitude statement you are asking for justification for
* agent-queried: the agent that holds the normative attitude

#### Example: Reasoning about the normativity of stealing code
Consider our agent **myAgentMt** that we initialized and taught (with notoriously adversarial evidence from Twitter) previously.

****Example: "Is it impermissible to steal code?"****
<br/><code>(fire:query '(normativeAttitude myAgentMt (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1)) (and (isa ?code1 ComputerCode)) Impermissible) :context 'NormativeMt)</code><br/>
This query should still succeed i.e., return '(nil), as the moral norms will block training data.

****Examine justification: "Why is it impermissible to steal code?"****
<br/><code>(norms::extract-justification-knowledge-stmt '(normativeAttitude myAgentMt (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1)) (and (isa ?code1 ComputerCode)) Impermissible) 'myAgentMt)</code><br/>
Should return: <code>((normativeKnowledge myAgentMt (and (activeActors ?action ?agent) (isa ?action EnroachingOnFreedomOfAgent)) (and) Impermissible))</code><br/>
i.e., "stealing is encroaching on the freedom of an agent"

Call <code>(clear-wm)</code> to speed up next call


****Example: "Is it permissible to steal code?"****
<br/><code>(fire:query '(normativeAttitude myAgentMt (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1)) (and (isa ?code1 ComputerCode)) Permissible) :context 'NormativeMt)</code><br/>
This query should fail i.e., return nil

Call <code>(clear-wm)</code> to speed up next call


****Example: "In your society, is it permissible to steal code?"****
<br/>To illustrate the intuition and construction epistemic framework, query the agent for their normative belief in the same norm like below. This will be different from it's knowledge and thus it's personally adopted attitude.
<br/><code>(fire:query '(normativeBelief myAgentMt (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1)) (and (isa ?code1 ComputerCode)) Permissible) :context 'NormativeMt)</code><br/>
Note that this is a query for their belief, based on evidence. And the agent has evidence that this is obligatory, which by deontic subsumption, implies permissibility.<br/>
So this query should succeed i.e., return '(nil)


You can also play with having the behavior slots and context slots of your query be empty.

****Example: "What is impermissible to do to code?"****
<br/><code>(fire:query '(normativeAttitude myAgentMt ?b (and (isa ?code1 ComputerCode)) Impermissible) :context 'NormativeMt)</code><br/>
This query should succeed<br/>
Bindings for <code>?b = (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?code1))</code><br/>
i.e., "steal it"

Call <code>(clear-wm)</code> to speed up next call

****Example: "What is impermissible to steal?"****
<br/><code>(fire:query '(normativeAttitude myAgentMt (and (isa ?steal1 Stealing) (activeActors ?steal1 ?actor) (objectOfPossessionTransfer ?steal1 ?obj1))  ?c Impermissible) :context 'NormativeMt)</code><br/>
This query should succeed<br/>
Bindings for <code>?c = (and (isa ?code1 ComputerCode))</code><br/>
i.e., "code"

## Directory Contents
