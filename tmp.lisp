
(defmethod initialize-instance :after ((executor executor) &rest initargs)
  (declare (ignore initargs))
  (setf (h executor) (/ (- (b executor) (a executor)) (n executor)))
  ;h = (b - a) / n 
  (setf (leafs executor) (* 2 (1- (n executor))))
  ;(setf (tree executor)
  (format t "h= ~A leafs: ~A" (h executor) (leafs executor)))

;  	Node _root;;
;	TreeIniter _tree;
;	Counter _counter;



(defmethod compute ((ex executor))
  (let ((counter (make-instance 'counter ))
	(root (make-instance 'node)))
    
    
    )
  







#|
;		_counter = new Counter();
					;(A) productions
		_counter.setCounterTo(parents.size());
		for(Node n : parents){
			if(n.getLeftChild().IS_FIRST_LEAF_NODE || n.getRightChild().IS_FIRST_LEAF_NODE){
				System.out.println(n.getName() + " - " + "A1");
				new Thread(new Production("A1", n, _counter)).start();
			}
			else if(n.getLeftChild().IS_LAST_LEAF_NODE || n.getRightChild().IS_LAST_LEAF_NODE){
				System.out.println(n.getName() + " - " + "AN");
				new Thread(new Production("AN", n, _counter)).start();
			}
			else{
				System.out.println(n.getName() + " - " + "A");
				new Thread(new Production("A", n, _counter)).start();
			}
		}
		while(!_counter.haveFinished());
		
		
System.out.println();
		
		
		//(A2), (E2) productions
		for(int i = _tree.getMaxLevel() - 2; i >= 1; i--){
			List<Node> nodes = _tree.getNodesByLevel(_root, i);
			nodes = _tree.removeChildren(nodes);
			
			_counter.setCounterTo(nodes.size());
			for(Node n : nodes)
				new Thread(new Production("A2, E2", n, _counter)).start();

			while(!_counter.haveFinished());
			
System.out.println();			
			
		}
		
		
		//(Aroot), (Eroot) productions
		_counter.setCounterTo(1);
		
		new Thread(new Production("Aroot, Eroot", _root, _counter)).start();
		
		while(!_counter.haveFinished());
		
		
		
		
		//(BS) productions
		for(int i = 0 ; i < _tree.getMaxLevel(); i++){
			List<Node> nodes = _tree.getNodesByLevel(_root, i);
			nodes = _tree.removeChildren(nodes);
			
			_counter.setCounterTo(nodes.size());
			for(Node n : nodes)
				new Thread(new Production("BS", n, _counter)).start();

			while(!_counter.haveFinished());
			
System.out.println();			
			
		}
		
		
		
		
		
		
	}

	private void printTree(Node node, int level) {
		node.setLevel(level);
		level++;
		
		if(node.getLeftChild() == null && node.getRightChild() == null)
			System.out.println("Leaf: " + node.getName());
		else
			System.out.println(node.getName() + ", leftChild: " + node.getLeftChild().getName() + ", rightChild: " + node.getRightChild().getName());
		
		if(node.getLeftChild() != null && node.getRightChild() != null){
			printTree(node.getLeftChild(), level);
			printTree(node.getRightChild(), level);
		}
	}
	

	
	
	
}
|#
