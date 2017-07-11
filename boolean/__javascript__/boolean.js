"use strict";
// Transcrypt'ed from Python, 2017-07-11 14:02:50
function boolean () {
   var __symbols__ = ['__py3.6__', '__esv 6__'];
    var __all__ = {};
    var __world__ = __all__;
    
    // Nested object creator, part of the nesting may already exist and have attributes
    var __nest__ = function (headObject, tailNames, value) {
        // In some cases this will be a global object, e.g. 'window'
        var current = headObject;
        
        if (tailNames != '') {  // Split on empty string doesn't give empty list
            // Find the last already created object in tailNames
            var tailChain = tailNames.split ('.');
            var firstNewIndex = tailChain.length;
            for (var index = 0; index < tailChain.length; index++) {
                if (!current.hasOwnProperty (tailChain [index])) {
                    firstNewIndex = index;
                    break;
                }
                current = current [tailChain [index]];
            }
            
            // Create the rest of the objects, if any
            for (var index = firstNewIndex; index < tailChain.length; index++) {
                current [tailChain [index]] = {};
                current = current [tailChain [index]];
            }
        }
        
        // Insert it new attributes, it may have been created earlier and have other attributes
        for (var attrib in value) {
            current [attrib] = value [attrib];          
        }       
    };
    __all__.__nest__ = __nest__;
    
    // Initialize module if not yet done and return its globals
    var __init__ = function (module) {
        if (!module.__inited__) {
            module.__all__.__init__ (module.__all__);
            module.__inited__ = true;
        }
        return module.__all__;
    };
    __all__.__init__ = __init__;
    
    
    
    
    // Since we want to assign functions, a = b.f should make b.f produce a bound function
    // So __get__ should be called by a property rather then a function
    // Factory __get__ creates one of three curried functions for func
    // Which one is produced depends on what's to the left of the dot of the corresponding JavaScript property
    var __get__ = function (self, func, quotedFuncName) {
        if (self) {
            if (self.hasOwnProperty ('__class__') || typeof self == 'string' || self instanceof String) {           // Object before the dot
                if (quotedFuncName) {                                   // Memoize call since fcall is on, by installing bound function in instance
                    Object.defineProperty (self, quotedFuncName, {      // Will override the non-own property, next time it will be called directly
                        value: function () {                            // So next time just call curry function that calls function
                            var args = [] .slice.apply (arguments);
                            return func.apply (null, [self] .concat (args));
                        },              
                        writable: true,
                        enumerable: true,
                        configurable: true
                    });
                }
                return function () {                                    // Return bound function, code dupplication for efficiency if no memoizing
                    var args = [] .slice.apply (arguments);             // So multilayer search prototype, apply __get__, call curry func that calls func
                    return func.apply (null, [self] .concat (args));
                };
            }
            else {                                                      // Class before the dot
                return func;                                            // Return static method
            }
        }
        else {                                                          // Nothing before the dot
            return func;                                                // Return free function
        }
    }
    __all__.__get__ = __get__;
        
    // Mother of all metaclasses        
    var py_metatype = {
        __name__: 'type',
        __bases__: [],
        
        // Overridable class creation worker
        __new__: function (meta, name, bases, attribs) {
            // Create the class cls, a functor, which the class creator function will return
            var cls = function () {                     // If cls is called with arg0, arg1, etc, it calls its __new__ method with [arg0, arg1, etc]
                var args = [] .slice.apply (arguments); // It has a __new__ method, not yet but at call time, since it is copied from the parent in the loop below
                return cls.__new__ (args);              // Each Python class directly or indirectly derives from object, which has the __new__ method
            };                                          // If there are no bases in the Python source, the compiler generates [object] for this parameter
            
            // Copy all methods, including __new__, properties and static attributes from base classes to new cls object
            // The new class object will simply be the prototype of its instances
            // JavaScript prototypical single inheritance will do here, since any object has only one class
            // This has nothing to do with Python multiple inheritance, that is implemented explictly in the copy loop below
            for (var index = bases.length - 1; index >= 0; index--) {   // Reversed order, since class vars of first base should win
                var base = bases [index];
                for (var attrib in base) {
                    var descrip = Object.getOwnPropertyDescriptor (base, attrib);
                    Object.defineProperty (cls, attrib, descrip);
                }           
            }
            
            // Add class specific attributes to the created cls object
            cls.__metaclass__ = meta;
            cls.__name__ = name;
            cls.__bases__ = bases;
            
            // Add own methods, properties and own static attributes to the created cls object
            for (var attrib in attribs) {
                var descrip = Object.getOwnPropertyDescriptor (attribs, attrib);
                Object.defineProperty (cls, attrib, descrip);
            }
            // Return created cls object
            return cls;
        }
    };
    py_metatype.__metaclass__ = py_metatype;
    __all__.py_metatype = py_metatype;
    
    // Mother of all classes
    var object = {
        __init__: function (self) {},
        
        __metaclass__: py_metatype, // By default, all classes have metaclass type, since they derive from object
        __name__: 'object',
        __bases__: [],
            
        // Object creator function, is inherited by all classes (so could be global)
        __new__: function (args) {  // Args are just the constructor args       
            // In JavaScript the Python class is the prototype of the Python object
            // In this way methods and static attributes will be available both with a class and an object before the dot
            // The descriptor produced by __get__ will return the right method flavor
            var instance = Object.create (this, {__class__: {value: this, enumerable: true}});
            

            // Call constructor
            this.__init__.apply (null, [instance] .concat (args));

            // Return constructed instance
            return instance;
        }   
    };
    __all__.object = object;
    
    // Class creator facade function, calls class creation worker
    var __class__ = function (name, bases, attribs, meta) {         // Parameter meta is optional
        if (meta == undefined) {
            meta = bases [0] .__metaclass__;
        }
                
        return meta.__new__ (meta, name, bases, attribs);
    }
    __all__.__class__ = __class__;
    
    // Define __pragma__ to preserve '<all>' and '</all>', since it's never generated as a function, must be done early, so here
    var __pragma__ = function () {};
    __all__.__pragma__ = __pragma__;
    
    	__nest__ (
		__all__,
		'org.transcrypt.__base__', {
			__all__: {
				__inited__: false,
				__init__: function (__all__) {
					var __Envir__ = __class__ ('__Envir__', [object], {
						get __init__ () {return __get__ (this, function (self) {
							self.interpreter_name = 'python';
							self.transpiler_name = 'transcrypt';
							self.transpiler_version = '3.6.34';
							self.target_subdir = '__javascript__';
						});}
					});
					var __envir__ = __Envir__ ();
					__pragma__ ('<all>')
						__all__.__Envir__ = __Envir__;
						__all__.__envir__ = __envir__;
					__pragma__ ('</all>')
				}
			}
		}
	);
	__nest__ (
		__all__,
		'org.transcrypt.__standard__', {
			__all__: {
				__inited__: false,
				__init__: function (__all__) {
					var Exception = __class__ ('Exception', [object], {
						get __init__ () {return __get__ (this, function (self) {
							var kwargs = dict ();
							if (arguments.length) {
								var __ilastarg0__ = arguments.length - 1;
								if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
									var __allkwargs0__ = arguments [__ilastarg0__--];
									for (var __attrib0__ in __allkwargs0__) {
										switch (__attrib0__) {
											case 'self': var self = __allkwargs0__ [__attrib0__]; break;
											default: kwargs [__attrib0__] = __allkwargs0__ [__attrib0__];
										}
									}
									delete kwargs.__kwargtrans__;
								}
								var args = tuple ([].slice.apply (arguments).slice (1, __ilastarg0__ + 1));
							}
							else {
								var args = tuple ();
							}
							self.__args__ = args;
							try {
								self.stack = kwargs.error.stack;
							}
							catch (__except0__) {
								self.stack = 'No stack trace available';
							}
						});},
						get __repr__ () {return __get__ (this, function (self) {
							if (len (self.__args__)) {
								return '{}{}'.format (self.__class__.__name__, repr (tuple (self.__args__)));
							}
							else {
								return '{}()'.format (self.__class__.__name__);
							}
						});},
						get __str__ () {return __get__ (this, function (self) {
							if (len (self.__args__) > 1) {
								return str (tuple (self.__args__));
							}
							else if (len (self.__args__)) {
								return str (self.__args__ [0]);
							}
							else {
								return '';
							}
						});}
					});
					var IterableError = __class__ ('IterableError', [Exception], {
						get __init__ () {return __get__ (this, function (self, error) {
							Exception.__init__ (self, "Can't iterate over non-iterable", __kwargtrans__ ({error: error}));
						});}
					});
					var StopIteration = __class__ ('StopIteration', [Exception], {
						get __init__ () {return __get__ (this, function (self, error) {
							Exception.__init__ (self, 'Iterator exhausted', __kwargtrans__ ({error: error}));
						});}
					});
					var ValueError = __class__ ('ValueError', [Exception], {
						get __init__ () {return __get__ (this, function (self, error) {
							Exception.__init__ (self, 'Erroneous value', __kwargtrans__ ({error: error}));
						});}
					});
					var KeyError = __class__ ('KeyError', [Exception], {
						get __init__ () {return __get__ (this, function (self, error) {
							Exception.__init__ (self, 'Invalid key', __kwargtrans__ ({error: error}));
						});}
					});
					var AssertionError = __class__ ('AssertionError', [Exception], {
						get __init__ () {return __get__ (this, function (self, message, error) {
							if (message) {
								Exception.__init__ (self, message, __kwargtrans__ ({error: error}));
							}
							else {
								Exception.__init__ (self, __kwargtrans__ ({error: error}));
							}
						});}
					});
					var NotImplementedError = __class__ ('NotImplementedError', [Exception], {
						get __init__ () {return __get__ (this, function (self, message, error) {
							Exception.__init__ (self, message, __kwargtrans__ ({error: error}));
						});}
					});
					var IndexError = __class__ ('IndexError', [Exception], {
						get __init__ () {return __get__ (this, function (self, message, error) {
							Exception.__init__ (self, message, __kwargtrans__ ({error: error}));
						});}
					});
					var AttributeError = __class__ ('AttributeError', [Exception], {
						get __init__ () {return __get__ (this, function (self, message, error) {
							Exception.__init__ (self, message, __kwargtrans__ ({error: error}));
						});}
					});
					var Warning = __class__ ('Warning', [Exception], {
					});
					var UserWarning = __class__ ('UserWarning', [Warning], {
					});
					var DeprecationWarning = __class__ ('DeprecationWarning', [Warning], {
					});
					var RuntimeWarning = __class__ ('RuntimeWarning', [Warning], {
					});
					var __sort__ = function (iterable, key, reverse) {
						if (typeof key == 'undefined' || (key != null && key .hasOwnProperty ("__kwargtrans__"))) {;
							var key = null;
						};
						if (typeof reverse == 'undefined' || (reverse != null && reverse .hasOwnProperty ("__kwargtrans__"))) {;
							var reverse = false;
						};
						if (arguments.length) {
							var __ilastarg0__ = arguments.length - 1;
							if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
								var __allkwargs0__ = arguments [__ilastarg0__--];
								for (var __attrib0__ in __allkwargs0__) {
									switch (__attrib0__) {
										case 'iterable': var iterable = __allkwargs0__ [__attrib0__]; break;
										case 'key': var key = __allkwargs0__ [__attrib0__]; break;
										case 'reverse': var reverse = __allkwargs0__ [__attrib0__]; break;
									}
								}
							}
						}
						else {
						}
						if (key) {
							iterable.sort ((function __lambda__ (a, b) {
								if (arguments.length) {
									var __ilastarg0__ = arguments.length - 1;
									if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
										var __allkwargs0__ = arguments [__ilastarg0__--];
										for (var __attrib0__ in __allkwargs0__) {
											switch (__attrib0__) {
												case 'a': var a = __allkwargs0__ [__attrib0__]; break;
												case 'b': var b = __allkwargs0__ [__attrib0__]; break;
											}
										}
									}
								}
								else {
								}
								return (key (a) > key (b) ? 1 : -(1));
							}));
						}
						else {
							iterable.sort ();
						}
						if (reverse) {
							iterable.reverse ();
						}
					};
					var sorted = function (iterable, key, reverse) {
						if (typeof key == 'undefined' || (key != null && key .hasOwnProperty ("__kwargtrans__"))) {;
							var key = null;
						};
						if (typeof reverse == 'undefined' || (reverse != null && reverse .hasOwnProperty ("__kwargtrans__"))) {;
							var reverse = false;
						};
						if (arguments.length) {
							var __ilastarg0__ = arguments.length - 1;
							if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
								var __allkwargs0__ = arguments [__ilastarg0__--];
								for (var __attrib0__ in __allkwargs0__) {
									switch (__attrib0__) {
										case 'iterable': var iterable = __allkwargs0__ [__attrib0__]; break;
										case 'key': var key = __allkwargs0__ [__attrib0__]; break;
										case 'reverse': var reverse = __allkwargs0__ [__attrib0__]; break;
									}
								}
							}
						}
						else {
						}
						if (py_typeof (iterable) == dict) {
							var result = copy (iterable.py_keys ());
						}
						else {
							var result = copy (iterable);
						}
						__sort__ (result, key, reverse);
						return result;
					};
					var map = function (func, iterable) {
						return function () {
							var __accu0__ = [];
							for (var item of iterable) {
								__accu0__.append (func (item));
							}
							return __accu0__;
						} ();
					};
					var filter = function (func, iterable) {
						if (func == null) {
							var func = bool;
						}
						return function () {
							var __accu0__ = [];
							for (var item of iterable) {
								if (func (item)) {
									__accu0__.append (item);
								}
							}
							return __accu0__;
						} ();
					};
					var __Terminal__ = __class__ ('__Terminal__', [object], {
						get __init__ () {return __get__ (this, function (self) {
							self.buffer = '';
							try {
								self.element = document.getElementById ('__terminal__');
							}
							catch (__except0__) {
								self.element = null;
							}
							if (self.element) {
								self.element.style.overflowX = 'auto';
								self.element.style.boxSizing = 'border-box';
								self.element.style.padding = '5px';
								self.element.innerHTML = '_';
							}
						});},
						get print () {return __get__ (this, function (self) {
							var sep = ' ';
							var end = '\n';
							if (arguments.length) {
								var __ilastarg0__ = arguments.length - 1;
								if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
									var __allkwargs0__ = arguments [__ilastarg0__--];
									for (var __attrib0__ in __allkwargs0__) {
										switch (__attrib0__) {
											case 'self': var self = __allkwargs0__ [__attrib0__]; break;
											case 'sep': var sep = __allkwargs0__ [__attrib0__]; break;
											case 'end': var end = __allkwargs0__ [__attrib0__]; break;
										}
									}
								}
								var args = tuple ([].slice.apply (arguments).slice (1, __ilastarg0__ + 1));
							}
							else {
								var args = tuple ();
							}
							self.buffer = '{}{}{}'.format (self.buffer, sep.join (function () {
								var __accu0__ = [];
								for (var arg of args) {
									__accu0__.append (str (arg));
								}
								return __accu0__;
							} ()), end).__getslice__ (-(4096), null, 1);
							if (self.element) {
								self.element.innerHTML = self.buffer.py_replace ('\n', '<br>');
								self.element.scrollTop = self.element.scrollHeight;
							}
							else {
								console.log (sep.join (function () {
									var __accu0__ = [];
									for (var arg of args) {
										__accu0__.append (str (arg));
									}
									return __accu0__;
								} ()));
							}
						});},
						get input () {return __get__ (this, function (self, question) {
							if (arguments.length) {
								var __ilastarg0__ = arguments.length - 1;
								if (arguments [__ilastarg0__] && arguments [__ilastarg0__].hasOwnProperty ("__kwargtrans__")) {
									var __allkwargs0__ = arguments [__ilastarg0__--];
									for (var __attrib0__ in __allkwargs0__) {
										switch (__attrib0__) {
											case 'self': var self = __allkwargs0__ [__attrib0__]; break;
											case 'question': var question = __allkwargs0__ [__attrib0__]; break;
										}
									}
								}
							}
							else {
							}
							self.print ('{}'.format (question), __kwargtrans__ ({end: ''}));
							var answer = window.prompt ('\n'.join (self.buffer.py_split ('\n').__getslice__ (-(16), null, 1)));
							self.print (answer);
							return answer;
						});}
					});
					var __terminal__ = __Terminal__ ();
					__pragma__ ('<all>')
						__all__.AssertionError = AssertionError;
						__all__.AttributeError = AttributeError;
						__all__.DeprecationWarning = DeprecationWarning;
						__all__.Exception = Exception;
						__all__.IndexError = IndexError;
						__all__.IterableError = IterableError;
						__all__.KeyError = KeyError;
						__all__.NotImplementedError = NotImplementedError;
						__all__.RuntimeWarning = RuntimeWarning;
						__all__.StopIteration = StopIteration;
						__all__.UserWarning = UserWarning;
						__all__.ValueError = ValueError;
						__all__.Warning = Warning;
						__all__.__Terminal__ = __Terminal__;
						__all__.__sort__ = __sort__;
						__all__.__terminal__ = __terminal__;
						__all__.filter = filter;
						__all__.map = map;
						__all__.sorted = sorted;
					__pragma__ ('</all>')
				}
			}
		}
	);
    var __call__ = function (/* <callee>, <this>, <params>* */) {   // Needed for __base__ and __standard__ if global 'opov' switch is on
        var args = [] .slice.apply (arguments);
        if (typeof args [0] == 'object' && '__call__' in args [0]) {        // Overloaded
            return args [0] .__call__ .apply (args [1], args.slice (2));
        }
        else {                                                              // Native
            return args [0] .apply (args [1], args.slice (2));
        }
    };
    __all__.__call__ = __call__;

    // Initialize non-nested modules __base__ and __standard__ and make its names available directly and via __all__
    // They can't do that itself, because they're regular Python modules
    // The compiler recognizes their names and generates them inline rather than nesting them
    // In this way it isn't needed to import them everywhere

    // __base__

    __nest__ (__all__, '', __init__ (__all__.org.transcrypt.__base__));
    var __envir__ = __all__.__envir__;

    // __standard__

    __nest__ (__all__, '', __init__ (__all__.org.transcrypt.__standard__));

    var Exception = __all__.Exception;
    var IterableError = __all__.IterableError;
    var StopIteration = __all__.StopIteration;
    var ValueError = __all__.ValueError;
    var KeyError = __all__.KeyError;
    var AssertionError = __all__.AssertionError;
    var NotImplementedError = __all__.NotImplementedError;
    var IndexError = __all__.IndexError;
    var AttributeError = __all__.AttributeError;

    // Warnings Exceptions
    var Warning = __all__.Warning;
    var UserWarning = __all__.UserWarning;
    var DeprecationWarning = __all__.DeprecationWarning;
    var RuntimeWarning = __all__.RuntimeWarning;

    var __sort__ = __all__.__sort__;
    var sorted = __all__.sorted;

    var map = __all__.map;
    var filter = __all__.filter;

    __all__.print = __all__.__terminal__.print;
    __all__.input = __all__.__terminal__.input;

    var __terminal__ = __all__.__terminal__;
    var print = __all__.print;
    var input = __all__.input;

    // Complete __envir__, that was created in __base__, for non-stub mode
    __envir__.executor_name = __envir__.transpiler_name;

    // Make make __main__ available in browser
    var __main__ = {__file__: ''};
    __all__.main = __main__;

    // Define current exception, there's at most one exception in the air at any time
    var __except__ = null;
    __all__.__except__ = __except__;
    
     // Creator of a marked dictionary, used to pass **kwargs parameter
    var __kwargtrans__ = function (anObject) {
        anObject.__kwargtrans__ = null; // Removable marker
        anObject.constructor = Object;
        return anObject;
    }
    __all__.__kwargtrans__ = __kwargtrans__;

    // 'Oneshot' dict promotor, used to enrich __all__ and help globals () return a true dict
    var __globals__ = function (anObject) {
        if (isinstance (anObject, dict)) {  // Don't attempt to promote (enrich) again, since it will make a copy
            return anObject;
        }
        else {
            return dict (anObject)
        }
    }
    __all__.__globals__ = __globals__
    
    // Partial implementation of super () .<methodName> (<params>)
    var __super__ = function (aClass, methodName) {        
        // Lean and fast, no C3 linearization, only call first implementation encountered
        // Will allow __super__ ('<methodName>') (self, <params>) rather than only <className>.<methodName> (self, <params>)
        
        for (var index = 0; index < aClass.__bases__.length; index++) {
            var base = aClass.__bases__ [index];
            if (methodName in base) {
               return base [methodName];
            }
        }

        throw new Exception ('Superclass method not found');    // !!! Improve!
    }
    __all__.__super__ = __super__
        
    // Python property installer function, no member since that would bloat classes
    var property = function (getter, setter) {  // Returns a property descriptor rather than a property
        if (!setter) {  // ??? Make setter optional instead of dummy?
            setter = function () {};
        }
        return {get: function () {return getter (this)}, set: function (value) {setter (this, value)}, enumerable: true};
    }
    __all__.property = property;
    
    // Conditional JavaScript property installer function, prevents redefinition of properties if multiple Transcrypt apps are on one page
    var __setProperty__ = function (anObject, name, descriptor) {
        if (!anObject.hasOwnProperty (name)) {
            Object.defineProperty (anObject, name, descriptor);
        }
    }
    __all__.__setProperty__ = __setProperty__
    
    // Assert function, call to it only generated when compiling with --dassert option
    function assert (condition, message) {  // Message may be undefined
        if (!condition) {
            throw AssertionError (message, new Error ());
        }
    }

    __all__.assert = assert;

    var __merge__ = function (object0, object1) {
        var result = {};
        for (var attrib in object0) {
            result [attrib] = object0 [attrib];
        }
        for (var attrib in object1) {
            result [attrib] = object1 [attrib];
        }
        return result;
    };
    __all__.__merge__ = __merge__;

    // Manipulating attributes by name
    
    var dir = function (obj) {
        var aList = [];
        for (var aKey in obj) {
            aList.push (aKey);
        }
        aList.sort ();
        return aList;
    };
    __all__.dir = dir;

    var setattr = function (obj, name, value) {
        obj [name] = value;
    };
    __all__.setattr = setattr;

    var getattr = function (obj, name) {
        return obj [name];
    };
    __all__.getattr= getattr;

    var hasattr = function (obj, name) {
        try {
            return name in obj;
        }
        catch (exception) {
            return false;
        }
    };
    __all__.hasattr = hasattr;

    var delattr = function (obj, name) {
        delete obj [name];
    };
    __all__.delattr = (delattr);

    // The __in__ function, used to mimic Python's 'in' operator
    // In addition to CPython's semantics, the 'in' operator is also allowed to work on objects, avoiding a counterintuitive separation between Python dicts and JavaScript objects
    // In general many Transcrypt compound types feature a deliberate blend of Python and JavaScript facilities, facilitating efficient integration with JavaScript libraries
    // If only Python objects and Python dicts are dealt with in a certain context, the more pythonic 'hasattr' is preferred for the objects as opposed to 'in' for the dicts
    var __in__ = function (element, container) {
        if (py_typeof (container) == dict) {        // Currently only implemented as an augmented JavaScript object
            return container.hasOwnProperty (element);
        }
        else {                                      // Parameter 'element' itself is an array, string or a plain, non-dict JavaScript object
            return (
                container.indexOf ?                 // If it has an indexOf
                container.indexOf (element) > -1 :  // it's an array or a string,
                container.hasOwnProperty (element)  // else it's a plain, non-dict JavaScript object
            );
        }
    };
    __all__.__in__ = __in__;

    // Find out if an attribute is special
    var __specialattrib__ = function (attrib) {
        return (attrib.startswith ('__') && attrib.endswith ('__')) || attrib == 'constructor' || attrib.startswith ('py_');
    };
    __all__.__specialattrib__ = __specialattrib__;

    // Len function for any object
    var len = function (anObject) {
        if (anObject) {
            var l = anObject.length;
            if (l == undefined) {
                var result = 0;
                for (var attrib in anObject) {
                    if (!__specialattrib__ (attrib)) {
                        result++;
                    }
                }
                return result;
            }
            else {
                return l;
            }
        }
        else {
            return 0;
        }
    };
    __all__.len = len;

    // General conversions

    function __i__ (any) {  //  Conversion to iterable
        return py_typeof (any) == dict ? any.py_keys () : any;
    }

    function __t__ (any) {  // Conversion to truthyness, __ ([1, 2, 3]) returns [1, 2, 3], needed for nonempty selection: l = list1 or list2]
        return (['boolean', 'number'] .indexOf (typeof any) >= 0 || any instanceof Function || len (any)) ? any : false;
        // JavaScript functions have a length attribute, denoting the number of parameters
        // Python objects are JavaScript functions, but their length doesn't matter, only their existence
        // By the term 'any instanceof Function' we make sure that Python objects aren't rejected when their length equals zero
    }
    __all__.__t__ = __t__;

    var bool = function (any) {     // Always truly returns a bool, rather than something truthy or falsy
        return !!__t__ (any);
    };
    bool.__name__ = 'bool';         // So it can be used as a type with a name
    __all__.bool = bool;

    var float = function (any) {
        if (any == 'inf') {
            return Infinity;
        }
        else if (any == '-inf') {
            return -Infinity;
        }
        else if (isNaN (parseFloat (any))) {    // Call to parseFloat needed to exclude '', ' ' etc.
            throw ValueError (new Error ());
        }
        else {
            return +any;
        }
    };
    float.__name__ = 'float';
    __all__.float = float;

    var int = function (any) {
        return float (any) | 0
    };
    int.__name__ = 'int';
    __all__.int = int;

    var py_typeof = function (anObject) {
        var aType = typeof anObject;
        if (aType == 'object') {    // Directly trying '__class__ in anObject' turns out to wreck anObject in Chrome if its a primitive
            try {
                return anObject.__class__;
            }
            catch (exception) {
                return aType;
            }
        }
        else {
            return (    // Odly, the braces are required here
                aType == 'boolean' ? bool :
                aType == 'string' ? str :
                aType == 'number' ? (anObject % 1 == 0 ? int : float) :
                null
            );
        }
    };
    __all__.py_typeof = py_typeof;

    var isinstance = function (anObject, classinfo) {
        function isA (queryClass) {
            if (queryClass == classinfo) {
                return true;
            }
            for (var index = 0; index < queryClass.__bases__.length; index++) {
                if (isA (queryClass.__bases__ [index], classinfo)) {
                    return true;
                }
            }
            return false;
        }

        if (classinfo instanceof Array) {   // Assume in most cases it isn't, then making it recursive rather than two functions saves a call
            for (var index = 0; index < classinfo.length; index++) {
                var aClass = classinfo [index];
                if (isinstance (anObject, aClass)) {
                    return true;
                }
            }
            return false;
        }

        try {                   // Most frequent use case first
            return '__class__' in anObject ? isA (anObject.__class__) : anObject instanceof classinfo;
        }
        catch (exception) {     // Using isinstance on primitives assumed rare
            var aType = py_typeof (anObject);
            return aType == classinfo || (aType == bool && classinfo == int);
        }
    };
    __all__.isinstance = isinstance;

    var callable = function (anObject) {
        if ( typeof anObject == 'object' && '__call__' in anObject ) {
            return true;
        }
        else {
            return typeof anObject === 'function';
        }
    };
    __all__.callable = callable;

    // Repr function uses __repr__ method, then __str__, then toString
    var repr = function (anObject) {
        try {
            return anObject.__repr__ ();
        }
        catch (exception) {
            try {
                return anObject.__str__ ();
            }
            catch (exception) { // anObject has no __repr__ and no __str__
                try {
                    if (anObject == null) {
                        return 'None';
                    }
                    else if (anObject.constructor == Object) {
                        var result = '{';
                        var comma = false;
                        for (var attrib in anObject) {
                            if (!__specialattrib__ (attrib)) {
                                if (attrib.isnumeric ()) {
                                    var attribRepr = attrib;                // If key can be interpreted as numerical, we make it numerical
                                }                                           // So we accept that '1' is misrepresented as 1
                                else {
                                    var attribRepr = '\'' + attrib + '\'';  // Alpha key in dict
                                }

                                if (comma) {
                                    result += ', ';
                                }
                                else {
                                    comma = true;
                                }
                                result += attribRepr + ': ' + repr (anObject [attrib]);
                            }
                        }
                        result += '}';
                        return result;
                    }
                    else {
                        return typeof anObject == 'boolean' ? anObject.toString () .capitalize () : anObject.toString ();
                    }
                }
                catch (exception) {
                    console.log ('ERROR: Could not evaluate repr (<object of type ' + typeof anObject + '>)');
                    console.log (exception);
                    return '???';
                }
            }
        }
    };
    __all__.repr = repr;

    // Char from Unicode or ASCII
    var chr = function (charCode) {
        return String.fromCharCode (charCode);
    };
    __all__.chr = chr;

    // Unicode or ASCII from char
    var ord = function (aChar) {
        return aChar.charCodeAt (0);
    };
    __all__.ord = ord;

    // Maximum of n numbers
    var max = Math.max;
    __all__.max = max;

    // Minimum of n numbers
    var min = Math.min;
    __all__.min = min;

    // Absolute value
    var abs = Math.abs;
    __all__.abs = abs;

    // Bankers rounding
    var round = function (number, ndigits) {
        if (ndigits) {
            var scale = Math.pow (10, ndigits);
            number *= scale;
        }

        var rounded = Math.round (number);
        if (rounded - number == 0.5 && rounded % 2) {   // Has rounded up to odd, should have rounded down to even
            rounded -= 1;
        }

        if (ndigits) {
            rounded /= scale;
        }

        return rounded;
    };
    __all__.round = round;

    // BEGIN unified iterator model

    function __jsUsePyNext__ () {       // Add as 'next' method to make Python iterator JavaScript compatible
        try {
            var result = this.__next__ ();
            return {value: result, done: false};
        }
        catch (exception) {
            return {value: undefined, done: true};
        }
    }

    function __pyUseJsNext__ () {       // Add as '__next__' method to make JavaScript iterator Python compatible
        var result = this.next ();
        if (result.done) {
            throw StopIteration (new Error ());
        }
        else {
            return result.value;
        }
    }

    function py_iter (iterable) {                   // Alias for Python's iter function, produces a universal iterator / iterable, usable in Python and JavaScript
        if (typeof iterable == 'string' || '__iter__' in iterable) {    // JavaScript Array or string or Python iterable (string has no 'in')
            var result = iterable.__iter__ ();                          // Iterator has a __next__
            result.next = __jsUsePyNext__;                              // Give it a next
        }
        else if ('selector' in iterable) {                              // Assume it's a JQuery iterator
            var result = list (iterable) .__iter__ ();                  // Has a __next__
            result.next = __jsUsePyNext__;                              // Give it a next
        }
        else if ('next' in iterable) {                                  // It's a JavaScript iterator already,  maybe a generator, has a next and may have a __next__
            var result = iterable
            if (! ('__next__' in result)) {                             // If there's no danger of recursion
                result.__next__ = __pyUseJsNext__;                      // Give it a __next__
            }
        }
        else if (Symbol.iterator in iterable) {                         // It's a JavaScript iterable such as a typed array, but not an iterator
            var result = iterable [Symbol.iterator] ();                 // Has a next
            result.__next__ = __pyUseJsNext__;                          // Give it a __next__
        }
        else {
            throw IterableError (new Error ()); // No iterator at all
        }
        result [Symbol.iterator] = function () {return result;};
        return result;
    }

    function py_next (iterator) {               // Called only in a Python context, could receive Python or JavaScript iterator
        try {                                   // Primarily assume Python iterator, for max speed
            var result = iterator.__next__ ();
        }
        catch (exception) {                     // JavaScript iterators are the exception here
            var result = iterator.next ();
            if (result.done) {
                throw StopIteration (new Error ());
            }
            else {
                return result.value;
            }
        }
        if (result == undefined) {
            throw StopIteration (new Error ());
        }
        else {
            return result;
        }
    }

    function __PyIterator__ (iterable) {
        this.iterable = iterable;
        this.index = 0;
    }

    __PyIterator__.prototype.__next__ = function () {
        if (this.index < this.iterable.length) {
            return this.iterable [this.index++];
        }
        else {
            throw StopIteration (new Error ());
        }
    };

    function __JsIterator__ (iterable) {
        this.iterable = iterable;
        this.index = 0;
    }

    __JsIterator__.prototype.next = function () {
        if (this.index < this.iterable.py_keys.length) {
            return {value: this.index++, done: false};
        }
        else {
            return {value: undefined, done: true};
        }
    };

    // END unified iterator model

    // Reversed function for arrays
    var py_reversed = function (iterable) {
        iterable = iterable.slice ();
        iterable.reverse ();
        return iterable;
    };
    __all__.py_reversed = py_reversed;

    // Zip method for arrays and strings
    var zip = function () {
        var args = [] .slice.call (arguments);
        for (var i = 0; i < args.length; i++) {
            if (typeof args [i] == 'string') {
                args [i] = args [i] .split ('');
            }
        }
        var shortest = args.length == 0 ? [] : args.reduce (    // Find shortest array in arguments
            function (array0, array1) {
                return array0.length < array1.length ? array0 : array1;
            }
        );
        return shortest.map (                   // Map each element of shortest array
            function (current, index) {         // To the result of this function
                return args.map (               // Map each array in arguments
                    function (current) {        // To the result of this function
                        return current [index]; // Namely it's index't entry
                    }
                );
            }
        );
    };
    __all__.zip = zip;

    // Range method, returning an array
    function range (start, stop, step) {
        if (stop == undefined) {
            // one param defined
            stop = start;
            start = 0;
        }
        if (step == undefined) {
            step = 1;
        }
        if ((step > 0 && start >= stop) || (step < 0 && start <= stop)) {
            return [];
        }
        var result = [];
        for (var i = start; step > 0 ? i < stop : i > stop; i += step) {
            result.push(i);
        }
        return result;
    };
    __all__.range = range;

    // Any, all and sum

    function any (iterable) {
        for (var index = 0; index < iterable.length; index++) {
            if (bool (iterable [index])) {
                return true;
            }
        }
        return false;
    }
    function all (iterable) {
        for (var index = 0; index < iterable.length; index++) {
            if (! bool (iterable [index])) {
                return false;
            }
        }
        return true;
    }
    function sum (iterable) {
        var result = 0;
        for (var index = 0; index < iterable.length; index++) {
            result += iterable [index];
        }
        return result;
    }

    __all__.any = any;
    __all__.all = all;
    __all__.sum = sum;

    // Enumerate method, returning a zipped list
    function enumerate (iterable) {
        return zip (range (len (iterable)), iterable);
    }
    __all__.enumerate = enumerate;

    // Shallow and deepcopy

    function copy (anObject) {
        if (anObject == null || typeof anObject == "object") {
            return anObject;
        }
        else {
            var result = {};
            for (var attrib in obj) {
                if (anObject.hasOwnProperty (attrib)) {
                    result [attrib] = anObject [attrib];
                }
            }
            return result;
        }
    }
    __all__.copy = copy;

    function deepcopy (anObject) {
        if (anObject == null || typeof anObject == "object") {
            return anObject;
        }
        else {
            var result = {};
            for (var attrib in obj) {
                if (anObject.hasOwnProperty (attrib)) {
                    result [attrib] = deepcopy (anObject [attrib]);
                }
            }
            return result;
        }
    }
    __all__.deepcopy = deepcopy;

    // List extensions to Array

    function list (iterable) {                                      // All such creators should be callable without new
        var instance = iterable ? [] .slice.apply (iterable) : [];  // Spread iterable, n.b. array.slice (), so array before dot
        // Sort is the normal JavaScript sort, Python sort is a non-member function
        return instance;
    }
    __all__.list = list;
    Array.prototype.__class__ = list;   // All arrays are lists (not only if constructed by the list ctor), unless constructed otherwise
    list.__name__ = 'list';

    /*
    Array.from = function (iterator) { // !!! remove
        result = [];
        for (item of iterator) {
            result.push (item);
        }
        return result;
    }
    */

    Array.prototype.__iter__ = function () {return new __PyIterator__ (this);};

    Array.prototype.__getslice__ = function (start, stop, step) {
        if (start < 0) {
            start = this.length + start;
        }

        if (stop == null) {
            stop = this.length;
        }
        else if (stop < 0) {
            stop = this.length + stop;
        }
        else if (stop > this.length) {
            stop = this.length;
        }

        var result = list ([]);
        for (var index = start; index < stop; index += step) {
            result.push (this [index]);
        }

        return result;
    };

    Array.prototype.__setslice__ = function (start, stop, step, source) {
        if (start < 0) {
            start = this.length + start;
        }

        if (stop == null) {
            stop = this.length;
        }
        else if (stop < 0) {
            stop = this.length + stop;
        }

        if (step == null) { // Assign to 'ordinary' slice, replace subsequence
            Array.prototype.splice.apply (this, [start, stop - start] .concat (source));
        }
        else {              // Assign to extended slice, replace designated items one by one
            var sourceIndex = 0;
            for (var targetIndex = start; targetIndex < stop; targetIndex += step) {
                this [targetIndex] = source [sourceIndex++];
            }
        }
    };

    Array.prototype.__repr__ = function () {
        if (this.__class__ == set && !this.length) {
            return 'set()';
        }

        var result = !this.__class__ || this.__class__ == list ? '[' : this.__class__ == tuple ? '(' : '{';

        for (var index = 0; index < this.length; index++) {
            if (index) {
                result += ', ';
            }
            result += repr (this [index]);
        }

        if (this.__class__ == tuple && this.length == 1) {
            result += ',';
        }

        result += !this.__class__ || this.__class__ == list ? ']' : this.__class__ == tuple ? ')' : '}';;
        return result;
    };

    Array.prototype.__str__ = Array.prototype.__repr__;

    Array.prototype.append = function (element) {
        this.push (element);
    };

    Array.prototype.clear = function () {
        this.length = 0;
    };

    Array.prototype.extend = function (aList) {
        this.push.apply (this, aList);
    };

    Array.prototype.insert = function (index, element) {
        this.splice (index, 0, element);
    };

    Array.prototype.remove = function (element) {
        var index = this.indexOf (element);
        if (index == -1) {
            throw ValueError (new Error ());
        }
        this.splice (index, 1);
    };

    Array.prototype.index = function (element) {
        return this.indexOf (element);
    };

    Array.prototype.py_pop = function (index) {
        if (index == undefined) {
            return this.pop ();  // Remove last element
        }
        else {
            return this.splice (index, 1) [0];
        }
    };

    Array.prototype.py_sort = function () {
        __sort__.apply  (null, [this].concat ([] .slice.apply (arguments)));    // Can't work directly with arguments
        // Python params: (iterable, key = None, reverse = False)
        // py_sort is called with the Transcrypt kwargs mechanism, and just passes the params on to __sort__
        // __sort__ is def'ed with the Transcrypt kwargs mechanism
    };

    Array.prototype.__add__ = function (aList) {
        return list (this.concat (aList));
    };

    Array.prototype.__mul__ = function (scalar) {
        var result = this;
        for (var i = 1; i < scalar; i++) {
            result = result.concat (this);
        }
        return result;
    };

    Array.prototype.__rmul__ = Array.prototype.__mul__;

    // Tuple extensions to Array

    function tuple (iterable) {
        var instance = iterable ? [] .slice.apply (iterable) : [];
        instance.__class__ = tuple; // Not all arrays are tuples
        return instance;
    }
    __all__.tuple = tuple;
    tuple.__name__ = 'tuple';

    // Set extensions to Array
    // N.B. Since sets are unordered, set operations will occasionally alter the 'this' array by sorting it

    function set (iterable) {
        var instance = [];
        if (iterable) {
            for (var index = 0; index < iterable.length; index++) {
                instance.add (iterable [index]);
            }


        }
        instance.__class__ = set;   // Not all arrays are sets
        return instance;
    }
    __all__.set = set;
    set.__name__ = 'set';

    Array.prototype.__bindexOf__ = function (element) { // Used to turn O (n^2) into O (n log n)
    // Since sorting is lex, compare has to be lex. This also allows for mixed lists

        element += '';

        var mindex = 0;
        var maxdex = this.length - 1;

        while (mindex <= maxdex) {
            var index = (mindex + maxdex) / 2 | 0;
            var middle = this [index] + '';

            if (middle < element) {
                mindex = index + 1;
            }
            else if (middle > element) {
                maxdex = index - 1;
            }
            else {
                return index;
            }
        }

        return -1;
    };

    Array.prototype.add = function (element) {
        if (this.indexOf (element) == -1) { // Avoid duplicates in set
            this.push (element);
        }
    };

    Array.prototype.discard = function (element) {
        var index = this.indexOf (element);
        if (index != -1) {
            this.splice (index, 1);
        }
    };

    Array.prototype.isdisjoint = function (other) {
        this.sort ();
        for (var i = 0; i < other.length; i++) {
            if (this.__bindexOf__ (other [i]) != -1) {
                return false;
            }
        }
        return true;
    };

    Array.prototype.issuperset = function (other) {
        this.sort ();
        for (var i = 0; i < other.length; i++) {
            if (this.__bindexOf__ (other [i]) == -1) {
                return false;
            }
        }
        return true;
    };

    Array.prototype.issubset = function (other) {
        return set (other.slice ()) .issuperset (this); // Sort copy of 'other', not 'other' itself, since it may be an ordered sequence
    };

    Array.prototype.union = function (other) {
        var result = set (this.slice () .sort ());
        for (var i = 0; i < other.length; i++) {
            if (result.__bindexOf__ (other [i]) == -1) {
                result.push (other [i]);
            }
        }
        return result;
    };

    Array.prototype.intersection = function (other) {
        this.sort ();
        var result = set ();
        for (var i = 0; i < other.length; i++) {
            if (this.__bindexOf__ (other [i]) != -1) {
                result.push (other [i]);
            }
        }
        return result;
    };

    Array.prototype.difference = function (other) {
        var sother = set (other.slice () .sort ());
        var result = set ();
        for (var i = 0; i < this.length; i++) {
            if (sother.__bindexOf__ (this [i]) == -1) {
                result.push (this [i]);
            }
        }
        return result;
    };

    Array.prototype.symmetric_difference = function (other) {
        return this.union (other) .difference (this.intersection (other));
    };

    Array.prototype.py_update = function () {   // O (n)
        var updated = [] .concat.apply (this.slice (), arguments) .sort ();
        this.clear ();
        for (var i = 0; i < updated.length; i++) {
            if (updated [i] != updated [i - 1]) {
                this.push (updated [i]);
            }
        }
    };

    Array.prototype.__eq__ = function (other) { // Also used for list
        if (this.length != other.length) {
            return false;
        }
        if (this.__class__ == set) {
            this.sort ();
            other.sort ();
        }
        for (var i = 0; i < this.length; i++) {
            if (this [i] != other [i]) {
                return false;
            }
        }
        return true;
    };

    Array.prototype.__ne__ = function (other) { // Also used for list
        return !this.__eq__ (other);
    };

    Array.prototype.__le__ = function (other) {
        return this.issubset (other);
    };

    Array.prototype.__ge__ = function (other) {
        return this.issuperset (other);
    };

    Array.prototype.__lt__ = function (other) {
        return this.issubset (other) && !this.issuperset (other);
    };

    Array.prototype.__gt__ = function (other) {
        return this.issuperset (other) && !this.issubset (other);
    };

    // String extensions

    function str (stringable) {
        try {
            return stringable.__str__ ();
        }
        catch (exception) {
            try {
                return repr (stringable);
            }
            catch (exception) {
                return String (stringable); // No new, so no permanent String object but a primitive in a temporary 'just in time' wrapper
            }
        }
    };
    __all__.str = str;

    String.prototype.__class__ = str;   // All strings are str
    str.__name__ = 'str';

    String.prototype.__iter__ = function () {new __PyIterator__ (this);};

    String.prototype.__repr__ = function () {
        return (this.indexOf ('\'') == -1 ? '\'' + this + '\'' : '"' + this + '"') .py_replace ('\t', '\\t') .py_replace ('\n', '\\n');
    };

    String.prototype.__str__ = function () {
        return this;
    };

    String.prototype.capitalize = function () {
        return this.charAt (0).toUpperCase () + this.slice (1);
    };

    String.prototype.endswith = function (suffix) {
        return suffix == '' || this.slice (-suffix.length) == suffix;
    };

    String.prototype.find  = function (sub, start) {
        return this.indexOf (sub, start);
    };

    String.prototype.__getslice__ = function (start, stop, step) {
        if (start < 0) {
            start = this.length + start;
        }

        if (stop == null) {
            stop = this.length;
        }
        else if (stop < 0) {
            stop = this.length + stop;
        }

        var result = '';
        if (step == 1) {
            result = this.substring (start, stop);
        }
        else {
            for (var index = start; index < stop; index += step) {
                result = result.concat (this.charAt(index));
            }
        }
        return result;
    }

    // Since it's worthwhile for the 'format' function to be able to deal with *args, it is defined as a property
    // __get__ will produce a bound function if there's something before the dot
    // Since a call using *args is compiled to e.g. <object>.<function>.apply (null, args), the function has to be bound already
    // Otherwise it will never be, because of the null argument
    // Using 'this' rather than 'null' contradicts the requirement to be able to pass bound functions around
    // The object 'before the dot' won't be available at call time in that case, unless implicitly via the function bound to it
    // While for Python methods this mechanism is generated by the compiler, for JavaScript methods it has to be provided manually
    // Call memoizing is unattractive here, since every string would then have to hold a reference to a bound format method
    __setProperty__ (String.prototype, 'format', {
        get: function () {return __get__ (this, function (self) {
            var args = tuple ([] .slice.apply (arguments).slice (1));
            var autoIndex = 0;
            return self.replace (/\{(\w*)\}/g, function (match, key) {
                if (key == '') {
                    key = autoIndex++;
                }
                if (key == +key) {  // So key is numerical
                    return args [key] == undefined ? match : str (args [key]);
                }
                else {              // Key is a string
                    for (var index = 0; index < args.length; index++) {
                        // Find first 'dict' that has that key and the right field
                        if (typeof args [index] == 'object' && args [index][key] != undefined) {
                            return str (args [index][key]); // Return that field field
                        }
                    }
                    return match;
                }
            });
        });},
        enumerable: true
    });

    String.prototype.isnumeric = function () {
        return !isNaN (parseFloat (this)) && isFinite (this);
    };

    String.prototype.join = function (strings) {
        return strings.join (this);
    };

    String.prototype.lower = function () {
        return this.toLowerCase ();
    };

    String.prototype.py_replace = function (old, aNew, maxreplace) {
        return this.split (old, maxreplace) .join (aNew);
    };

    String.prototype.lstrip = function () {
        return this.replace (/^\s*/g, '');
    };

    String.prototype.rfind = function (sub, start) {
        return this.lastIndexOf (sub, start);
    };

    String.prototype.rsplit = function (sep, maxsplit) {    // Combination of general whitespace sep and positive maxsplit neither supported nor checked, expensive and rare
        if (sep == undefined || sep == null) {
            sep = /\s+/;
            var stripped = this.strip ();
        }
        else {
            var stripped = this;
        }

        if (maxsplit == undefined || maxsplit == -1) {
            return stripped.split (sep);
        }
        else {
            var result = stripped.split (sep);
            if (maxsplit < result.length) {
                var maxrsplit = result.length - maxsplit;
                return [result.slice (0, maxrsplit) .join (sep)] .concat (result.slice (maxrsplit));
            }
            else {
                return result;
            }
        }
    };

    String.prototype.rstrip = function () {
        return this.replace (/\s*$/g, '');
    };

    String.prototype.py_split = function (sep, maxsplit) {  // Combination of general whitespace sep and positive maxsplit neither supported nor checked, expensive and rare
        if (sep == undefined || sep == null) {
            sep = /\s+/;
            var stripped = this.strip ();
        }
        else {
            var stripped = this;
        }

        if (maxsplit == undefined || maxsplit == -1) {
            return stripped.split (sep);
        }
        else {
            var result = stripped.split (sep);
            if (maxsplit < result.length) {
                return result.slice (0, maxsplit).concat ([result.slice (maxsplit).join (sep)]);
            }
            else {
                return result;
            }
        }
    };

    String.prototype.startswith = function (prefix) {
        return this.indexOf (prefix) == 0;
    };

    String.prototype.strip = function () {
        return this.trim ();
    };

    String.prototype.upper = function () {
        return this.toUpperCase ();
    };

    String.prototype.__mul__ = function (scalar) {
        var result = this;
        for (var i = 1; i < scalar; i++) {
            result = result + this;
        }
        return result;
    };

    String.prototype.__rmul__ = String.prototype.__mul__;

    // Dict extensions to object

    function __keys__ () {
        var keys = [];
        for (var attrib in this) {
            if (!__specialattrib__ (attrib)) {
                keys.push (attrib);
            }
        }
        return keys;
    }

    function __items__ () {
        var items = [];
        for (var attrib in this) {
            if (!__specialattrib__ (attrib)) {
                items.push ([attrib, this [attrib]]);
            }
        }
        return items;
    }

    function __del__ (key) {
        delete this [key];
    }

    function __clear__ () {
        for (var attrib in this) {
            delete this [attrib];
        }
    }

    function __getdefault__ (aKey, aDefault) {  // Each Python object already has a function called __get__, so we call this one __getdefault__
        var result = this [aKey];
        return result == undefined ? (aDefault == undefined ? null : aDefault) : result;
    }

    function __setdefault__ (aKey, aDefault) {
        var result = this [aKey];
        if (result != undefined) {
            return result;
        }
        var val = aDefault == undefined ? null : aDefault;
        this [aKey] = val;
        return val;
    }

    function __pop__ (aKey, aDefault) {
        var result = this [aKey];
        if (result != undefined) {
            delete this [aKey];
            return result;
        } else {
            // Identify check because user could pass None
            if ( aDefault === undefined ) {
                throw KeyError (aKey, new Error());
            }
        }
        return aDefault;
    }
    
    function __popitem__ () {
        var aKey = Object.keys (this) [0];
        if (aKey == null) {
            throw KeyError (aKey, new Error ());
        }
        var result = tuple ([aKey, this [aKey]]);
        delete this [aKey];
        return result;
    }
    
    function __update__ (aDict) {
        for (var aKey in aDict) {
            this [aKey] = aDict [aKey];
        }
    }
    
    function __values__ () {
        var values = [];
        for (var attrib in this) {
            if (!__specialattrib__ (attrib)) {
                values.push (this [attrib]);
            }
        }
        return values;

    }
    
    function __dgetitem__ (aKey) {
        return this [aKey];
    }
    
    function __dsetitem__ (aKey, aValue) {
        this [aKey] = aValue;
    }

    function dict (objectOrPairs) {
        var instance = {};
        if (!objectOrPairs || objectOrPairs instanceof Array) { // It's undefined or an array of pairs
            if (objectOrPairs) {
                for (var index = 0; index < objectOrPairs.length; index++) {
                    var pair = objectOrPairs [index];
                    if ( !(pair instanceof Array) || pair.length != 2) {
                        throw ValueError(
                            "dict update sequence element #" + index +
                            " has length " + pair.length +
                            "; 2 is required", new Error());
                    }
                    var key = pair [0];
                    var val = pair [1];
                    if (!(objectOrPairs instanceof Array) && objectOrPairs instanceof Object) {
                         // User can potentially pass in an object
                         // that has a hierarchy of objects. This
                         // checks to make sure that these objects
                         // get converted to dict objects instead of
                         // leaving them as js objects.
                         
                         if (!isinstance (objectOrPairs, dict)) {
                             val = dict (val);
                         }
                    }
                    instance [key] = val;
                }
            }
        }
        else {
            if (isinstance (objectOrPairs, dict)) {
                // Passed object is a dict already so we need to be a little careful
                // N.B. - this is a shallow copy per python std - so
                // it is assumed that children have already become
                // python objects at some point.
                
                var aKeys = objectOrPairs.py_keys ();
                for (var index = 0; index < aKeys.length; index++ ) {
                    var key = aKeys [index];
                    instance [key] = objectOrPairs [key];
                }
            } else if (objectOrPairs instanceof Object) {
                // Passed object is a JavaScript object but not yet a dict, don't copy it
                instance = objectOrPairs;
            } else {
                // We have already covered Array so this indicates
                // that the passed object is not a js object - i.e.
                // it is an int or a string, which is invalid.
                
                throw ValueError ("Invalid type of object for dict creation", new Error ());
            }
        }

        // Trancrypt interprets e.g. {aKey: 'aValue'} as a Python dict literal rather than a JavaScript object literal
        // So dict literals rather than bare Object literals will be passed to JavaScript libraries
        // Some JavaScript libraries call all enumerable callable properties of an object that's passed to them
        // So the properties of a dict should be non-enumerable
        __setProperty__ (instance, '__class__', {value: dict, enumerable: false, writable: true});
        __setProperty__ (instance, 'py_keys', {value: __keys__, enumerable: false});
        __setProperty__ (instance, '__iter__', {value: function () {new __PyIterator__ (this.py_keys ());}, enumerable: false});
        __setProperty__ (instance, Symbol.iterator, {value: function () {new __JsIterator__ (this.py_keys ());}, enumerable: false});
        __setProperty__ (instance, 'py_items', {value: __items__, enumerable: false});
        __setProperty__ (instance, 'py_del', {value: __del__, enumerable: false});
        __setProperty__ (instance, 'py_clear', {value: __clear__, enumerable: false});
        __setProperty__ (instance, 'py_get', {value: __getdefault__, enumerable: false});
        __setProperty__ (instance, 'py_setdefault', {value: __setdefault__, enumerable: false});
        __setProperty__ (instance, 'py_pop', {value: __pop__, enumerable: false});
        __setProperty__ (instance, 'py_popitem', {value: __popitem__, enumerable: false});
        __setProperty__ (instance, 'py_update', {value: __update__, enumerable: false});
        __setProperty__ (instance, 'py_values', {value: __values__, enumerable: false});
        __setProperty__ (instance, '__getitem__', {value: __dgetitem__, enumerable: false});    // Needed since compound keys necessarily
        __setProperty__ (instance, '__setitem__', {value: __dsetitem__, enumerable: false});    // trigger overloading to deal with slices
        return instance;
    }

    __all__.dict = dict;
    dict.__name__ = 'dict';
    
    // Docstring setter

    function __setdoc__ (docString) {
        this.__doc__ = docString;
        return this;
    }

    // Python classes, methods and functions are all translated to JavaScript functions
    __setProperty__ (Function.prototype, '__setdoc__', {value: __setdoc__, enumerable: false});

    // General operator overloading, only the ones that make most sense in matrix and complex operations

    var __neg__ = function (a) {
        if (typeof a == 'object' && '__neg__' in a) {
            return a.__neg__ ();
        }
        else {
            return -a;
        }
    };
    __all__.__neg__ = __neg__;

    var __matmul__ = function (a, b) {
        return a.__matmul__ (b);
    };
    __all__.__matmul__ = __matmul__;

    var __pow__ = function (a, b) {
        if (typeof a == 'object' && '__pow__' in a) {
            return a.__pow__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rpow__ (a);
        }
        else {
            return Math.pow (a, b);
        }
    };
    __all__.pow = __pow__;

    var __jsmod__ = function (a, b) {
        if (typeof a == 'object' && '__mod__' in a) {
            return a.__mod__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rmod__ (a);
        }
        else {
            return a % b;
        }
    };
    __all__.__jsmod__ = __jsmod__;
    
    var __mod__ = function (a, b) {
        if (typeof a == 'object' && '__mod__' in a) {
            return a.__mod__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rmod__ (a);
        }
        else {
            return ((a % b) + b) % b;
        }
    };
    __all__.mod = __mod__;

    // Overloaded binary arithmetic
    
    var __mul__ = function (a, b) {
        if (typeof a == 'object' && '__mul__' in a) {
            return a.__mul__ (b);
        }
        else if (typeof b == 'object' && '__rmul__' in b) {
            return b.__rmul__ (a);
        }
        else if (typeof a == 'string') {
            return a.__mul__ (b);
        }
        else if (typeof b == 'string') {
            return b.__rmul__ (a);
        }
        else {
            return a * b;
        }
    };
    __all__.__mul__ = __mul__;

    var __div__ = function (a, b) {
        if (typeof a == 'object' && '__div__' in a) {
            return a.__div__ (b);
        }
        else if (typeof b == 'object' && '__rdiv__' in b) {
            return b.__rdiv__ (a);
        }
        else {
            return a / b;
        }
    };
    __all__.__div__ = __div__;

    var __add__ = function (a, b) {
        if (typeof a == 'object' && '__add__' in a) {
            return a.__add__ (b);
        }
        else if (typeof b == 'object' && '__radd__' in b) {
            return b.__radd__ (a);
        }
        else {
            return a + b;
        }
    };
    __all__.__add__ = __add__;

    var __sub__ = function (a, b) {
        if (typeof a == 'object' && '__sub__' in a) {
            return a.__sub__ (b);
        }
        else if (typeof b == 'object' && '__rsub__' in b) {
            return b.__rsub__ (a);
        }
        else {
            return a - b;
        }
    };
    __all__.__sub__ = __sub__;

    // Overloaded binary bitwise
    
    var __lshift__ = function (a, b) {
        if (typeof a == 'object' && '__lshift__' in a) {
            return a.__lshift__ (b);
        }
        else if (typeof b == 'object' && '__rlshift__' in b) {
            return b.__rlshift__ (a);
        }
        else {
            return a << b;
        }
    };
    __all__.__lshift__ = __lshift__;

    var __rshift__ = function (a, b) {
        if (typeof a == 'object' && '__rshift__' in a) {
            return a.__rshift__ (b);
        }
        else if (typeof b == 'object' && '__rrshift__' in b) {
            return b.__rrshift__ (a);
        }
        else {
            return a >> b;
        }
    };
    __all__.__rshift__ = __rshift__;

    var __or__ = function (a, b) {
        if (typeof a == 'object' && '__or__' in a) {
            return a.__or__ (b);
        }
        else if (typeof b == 'object' && '__ror__' in b) {
            return b.__ror__ (a);
        }
        else {
            return a | b;
        }
    };
    __all__.__or__ = __or__;

    var __xor__ = function (a, b) {
        if (typeof a == 'object' && '__xor__' in a) {
            return a.__xor__ (b);
        }
        else if (typeof b == 'object' && '__rxor__' in b) {
            return b.__rxor__ (a);
        }
        else {
            return a ^ b;
        }
    };
    __all__.__xor__ = __xor__;

    var __and__ = function (a, b) {
        if (typeof a == 'object' && '__and__' in a) {
            return a.__and__ (b);
        }
        else if (typeof b == 'object' && '__rand__' in b) {
            return b.__rand__ (a);
        }
        else {
            return a & b;
        }
    };
    __all__.__and__ = __and__;    
        
    // Overloaded binary compare
    
    var __eq__ = function (a, b) {
        if (typeof a == 'object' && '__eq__' in a) {
            return a.__eq__ (b);
        }
        else {
            return a == b;
        }
    };
    __all__.__eq__ = __eq__;

    var __ne__ = function (a, b) {
        if (typeof a == 'object' && '__ne__' in a) {
            return a.__ne__ (b);
        }
        else {
            return a != b
        }
    };
    __all__.__ne__ = __ne__;

    var __lt__ = function (a, b) {
        if (typeof a == 'object' && '__lt__' in a) {
            return a.__lt__ (b);
        }
        else {
            return a < b;
        }
    };
    __all__.__lt__ = __lt__;

    var __le__ = function (a, b) {
        if (typeof a == 'object' && '__le__' in a) {
            return a.__le__ (b);
        }
        else {
            return a <= b;
        }
    };
    __all__.__le__ = __le__;

    var __gt__ = function (a, b) {
        if (typeof a == 'object' && '__gt__' in a) {
            return a.__gt__ (b);
        }
        else {
            return a > b;
        }
    };
    __all__.__gt__ = __gt__;

    var __ge__ = function (a, b) {
        if (typeof a == 'object' && '__ge__' in a) {
            return a.__ge__ (b);
        }
        else {
            return a >= b;
        }
    };
    __all__.__ge__ = __ge__;
    
    // Overloaded augmented general
    
    var __imatmul__ = function (a, b) {
        if ('__imatmul__' in a) {
            return a.__imatmul__ (b);
        }
        else {
            return a.__matmul__ (b);
        }
    };
    __all__.__imatmul__ = __imatmul__;

    var __ipow__ = function (a, b) {
        if (typeof a == 'object' && '__pow__' in a) {
            return a.__ipow__ (b);
        }
        else if (typeof a == 'object' && '__ipow__' in a) {
            return a.__pow__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rpow__ (a);
        }
        else {
            return Math.pow (a, b);
        }
    };
    __all__.ipow = __ipow__;

    var __ijsmod__ = function (a, b) {
        if (typeof a == 'object' && '__imod__' in a) {
            return a.__ismod__ (b);
        }
        else if (typeof a == 'object' && '__mod__' in a) {
            return a.__mod__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rmod__ (a);
        }
        else {
            return a % b;
        }
    };
    __all__.ijsmod__ = __ijsmod__;
    
    var __imod__ = function (a, b) {
        if (typeof a == 'object' && '__imod__' in a) {
            return a.__imod__ (b);
        }
        else if (typeof a == 'object' && '__mod__' in a) {
            return a.__mod__ (b);
        }
        else if (typeof b == 'object' && '__rpow__' in b) {
            return b.__rmod__ (a);
        }
        else {
            return ((a % b) + b) % b;
        }
    };
    __all__.imod = __imod__;
    
    // Overloaded augmented arithmetic
    
    var __imul__ = function (a, b) {
        if (typeof a == 'object' && '__imul__' in a) {
            return a.__imul__ (b);
        }
        else if (typeof a == 'object' && '__mul__' in a) {
            return a = a.__mul__ (b);
        }
        else if (typeof b == 'object' && '__rmul__' in b) {
            return a = b.__rmul__ (a);
        }
        else if (typeof a == 'string') {
            return a = a.__mul__ (b);
        }
        else if (typeof b == 'string') {
            return a = b.__rmul__ (a);
        }
        else {
            return a *= b;
        }
    };
    __all__.__imul__ = __imul__;

    var __idiv__ = function (a, b) {
        if (typeof a == 'object' && '__idiv__' in a) {
            return a.__idiv__ (b);
        }
        else if (typeof a == 'object' && '__div__' in a) {
            return a = a.__div__ (b);
        }
        else if (typeof b == 'object' && '__rdiv__' in b) {
            return a = b.__rdiv__ (a);
        }
        else {
            return a /= b;
        }
    };
    __all__.__idiv__ = __idiv__;

    var __iadd__ = function (a, b) {
        if (typeof a == 'object' && '__iadd__' in a) {
            return a.__iadd__ (b);
        }
        else if (typeof a == 'object' && '__add__' in a) {
            return a = a.__add__ (b);
        }
        else if (typeof b == 'object' && '__radd__' in b) {
            return a = b.__radd__ (a);
        }
        else {
            return a += b;
        }
    };
    __all__.__iadd__ = __iadd__;

    var __isub__ = function (a, b) {
        if (typeof a == 'object' && '__isub__' in a) {
            return a.__isub__ (b);
        }
        else if (typeof a == 'object' && '__sub__' in a) {
            return a = a.__sub__ (b);
        }
        else if (typeof b == 'object' && '__rsub__' in b) {
            return a = b.__rsub__ (a);
        }
        else {
            return a -= b;
        }
    };
    __all__.__isub__ = __isub__;

    // Overloaded augmented bitwise
    
    var __ilshift__ = function (a, b) {
        if (typeof a == 'object' && '__ilshift__' in a) {
            return a.__ilshift__ (b);
        }
        else if (typeof a == 'object' && '__lshift__' in a) {
            return a = a.__lshift__ (b);
        }
        else if (typeof b == 'object' && '__rlshift__' in b) {
            return a = b.__rlshift__ (a);
        }
        else {
            return a <<= b;
        }
    };
    __all__.__ilshift__ = __ilshift__;

    var __irshift__ = function (a, b) {
        if (typeof a == 'object' && '__irshift__' in a) {
            return a.__irshift__ (b);
        }
        else if (typeof a == 'object' && '__rshift__' in a) {
            return a = a.__rshift__ (b);
        }
        else if (typeof b == 'object' && '__rrshift__' in b) {
            return a = b.__rrshift__ (a);
        }
        else {
            return a >>= b;
        }
    };
    __all__.__irshift__ = __irshift__;

    var __ior__ = function (a, b) {
        if (typeof a == 'object' && '__ior__' in a) {
            return a.__ior__ (b);
        }
        else if (typeof a == 'object' && '__or__' in a) {
            return a = a.__or__ (b);
        }
        else if (typeof b == 'object' && '__ror__' in b) {
            return a = b.__ror__ (a);
        }
        else {
            return a |= b;
        }
    };
    __all__.__ior__ = __ior__;

    var __ixor__ = function (a, b) {
        if (typeof a == 'object' && '__ixor__' in a) {
            return a.__ixor__ (b);
        }
        else if (typeof a == 'object' && '__xor__' in a) {
            return a = a.__xor__ (b);
        }
        else if (typeof b == 'object' && '__rxor__' in b) {
            return a = b.__rxor__ (a);
        }
        else {
            return a ^= b;
        }
    };
    __all__.__ixor__ = __ixor__;

    var __iand__ = function (a, b) {
        if (typeof a == 'object' && '__iand__' in a) {
            return a.__iand__ (b);
        }
        else if (typeof a == 'object' && '__and__' in a) {
            return a = a.__and__ (b);
        }
        else if (typeof b == 'object' && '__rand__' in b) {
            return a = b.__rand__ (a);
        }
        else {
            return a &= b;
        }
    };
    __all__.__iand__ = __iand__;
    
    // Indices and slices

    var __getitem__ = function (container, key) {                           // Slice c.q. index, direct generated call to runtime switch
        if (typeof container == 'object' && '__getitem__' in container) {
            return container.__getitem__ (key);                             // Overloaded on container
        }
        else {
            return container [key];                                         // Container must support bare JavaScript brackets
        }
    };
    __all__.__getitem__ = __getitem__;

    var __setitem__ = function (container, key, value) {                    // Slice c.q. index, direct generated call to runtime switch
        if (typeof container == 'object' && '__setitem__' in container) {
            container.__setitem__ (key, value);                             // Overloaded on container
        }
        else {
            container [key] = value;                                        // Container must support bare JavaScript brackets
        }
    };
    __all__.__setitem__ = __setitem__;

    var __getslice__ = function (container, lower, upper, step) {           // Slice only, no index, direct generated call to runtime switch
        if (typeof container == 'object' && '__getitem__' in container) {
            return container.__getitem__ ([lower, upper, step]);            // Container supports overloaded slicing c.q. indexing
        }
        else {
            return container.__getslice__ (lower, upper, step);             // Container only supports slicing injected natively in prototype
        }
    };
    __all__.__getslice__ = __getslice__;

    var __setslice__ = function (container, lower, upper, step, value) {    // Slice, no index, direct generated call to runtime switch
        if (typeof container == 'object' && '__setitem__' in container) {
            container.__setitem__ ([lower, upper, step], value);            // Container supports overloaded slicing c.q. indexing
        }
        else {
            container.__setslice__ (lower, upper, step, value);             // Container only supports slicing injected natively in prototype
        }
    };
    __all__.__setslice__ = __setslice__;

	__nest__ (
		__all__,
		'__future__', {
			__all__: {
				__inited__: false,
				__init__: function (__all__) {
					var all_feature_names = list ([]);
					__pragma__ ('<all>')
						__all__.all_feature_names = all_feature_names;
					__pragma__ ('</all>')
				}
			}
		}
	);
	__nest__ (
		__all__,
		'boolean.boolean', {
			__all__: {
				__inited__: false,
				__init__: function (__all__) {
					var inspect = {};
					var itertools = {};
					var absolute_import = __init__ (__world__.__future__).absolute_import;
					var unicode_literals = __init__ (__world__.__future__).unicode_literals;
					var print_function = __init__ (__world__.__future__).print_function;
					__nest__ (inspect, '', __init__ (__world__.inspect));
					__nest__ (itertools, '', __init__ (__world__.itertools));
					var TRACE_PARSE = false;
					var TOKEN_AND = 1;
					var TOKEN_OR = 2;
					var TOKEN_NOT = 3;
					var TOKEN_LPAR = 4;
					var TOKEN_RPAR = 5;
					var TOKEN_TRUE = 6;
					var TOKEN_FALSE = 7;
					var TOKEN_SYMBOL = 8;
					var TOKEN_TYPES = dict ([[TOKEN_AND, 'AND'], [TOKEN_OR, 'OR'], [TOKEN_NOT, 'NOT'], [TOKEN_LPAR, '('], [TOKEN_RPAR, ')'], [TOKEN_TRUE, 'TRUE'], [TOKEN_FALSE, 'FALSE'], [TOKEN_SYMBOL, 'SYMBOL']]);
					var PARSE_UNKNOWN_TOKEN = 1;
					var PARSE_UNBALANCED_CLOSING_PARENS = 2;
					var PARSE_INVALID_EXPRESSION = 3;
					var PARSE_INVALID_NESTING = 4;
					var PARSE_INVALID_SYMBOL_SEQUENCE = 5;
					var PARSE_ERRORS = dict ([[PARSE_UNKNOWN_TOKEN, 'Unknown token'], [PARSE_UNBALANCED_CLOSING_PARENS, 'Unbalanced parenthesis'], [PARSE_INVALID_EXPRESSION, 'Invalid expression'], [PARSE_INVALID_NESTING, 'Invalid expression nesting such as (AND xx)'], [PARSE_INVALID_SYMBOL_SEQUENCE, 'Invalid symbols sequence such as (A B)']]);
					var ParseError = __class__ ('ParseError', [Exception], {
						get __init__ () {return __get__ (this, function (self, token_type, token_string, position, error_code) {
							if (typeof token_type == 'undefined' || (token_type != null && token_type .hasOwnProperty ("__kwargtrans__"))) {;
								var token_type = null;
							};
							if (typeof token_string == 'undefined' || (token_string != null && token_string .hasOwnProperty ("__kwargtrans__"))) {;
								var token_string = '';
							};
							if (typeof position == 'undefined' || (position != null && position .hasOwnProperty ("__kwargtrans__"))) {;
								var position = -(1);
							};
							if (typeof error_code == 'undefined' || (error_code != null && error_code .hasOwnProperty ("__kwargtrans__"))) {;
								var error_code = 0;
							};
							self.token_type = token_type;
							self.token_string = token_string;
							self.position = position;
							self.error_code = error_code;
						});},
						get __str__ () {return __get__ (this, function (self) {
							var args = tuple ([].slice.apply (arguments).slice (1));
							var emsg = PARSE_ERRORS.py_get (self.error_code, 'Unknown parsing error');
							var tstr = '';
							if (self.token_string) {
								var tstr = __mod__ (' for token: "%s"', self.token_string);
							}
							var pos = '';
							if (self.position > 0) {
								var pos = __mod__ (' at position: %d', self.position);
							}
							return '{emsg}{tstr}{pos}'.format (__kwargtrans__ (locals ()));
						});}
					});
					var BooleanAlgebra = __class__ ('BooleanAlgebra', [object], {
						get __init__ () {return __get__ (this, function (self, TRUE_class, FALSE_class, Symbol_class, NOT_class, AND_class, OR_class) {
							if (typeof TRUE_class == 'undefined' || (TRUE_class != null && TRUE_class .hasOwnProperty ("__kwargtrans__"))) {;
								var TRUE_class = null;
							};
							if (typeof FALSE_class == 'undefined' || (FALSE_class != null && FALSE_class .hasOwnProperty ("__kwargtrans__"))) {;
								var FALSE_class = null;
							};
							if (typeof Symbol_class == 'undefined' || (Symbol_class != null && Symbol_class .hasOwnProperty ("__kwargtrans__"))) {;
								var Symbol_class = null;
							};
							if (typeof NOT_class == 'undefined' || (NOT_class != null && NOT_class .hasOwnProperty ("__kwargtrans__"))) {;
								var NOT_class = null;
							};
							if (typeof AND_class == 'undefined' || (AND_class != null && AND_class .hasOwnProperty ("__kwargtrans__"))) {;
								var AND_class = null;
							};
							if (typeof OR_class == 'undefined' || (OR_class != null && OR_class .hasOwnProperty ("__kwargtrans__"))) {;
								var OR_class = null;
							};
							self.TRUE = TRUE_class || _TRUE;
							self.TRUE = self.TRUE ();
							self.FALSE = TRUE_class || _FALSE;
							self.FALSE = self.FALSE ();
							self.TRUE.dual = self.FALSE;
							self.FALSE.dual = self.TRUE;
							self.NOT = NOT_class || NOT;
							self.AND = AND_class || AND;
							self.OR = OR_class || OR;
							self.Symbol = Symbol_class || Symbol;
							var tf_nao = dict ({'TRUE': self.TRUE, 'FALSE': self.FALSE, 'NOT': self.NOT, 'AND': self.AND, 'OR': self.OR, 'Symbol': self.Symbol});
							self._cross_refs (tf_nao);
						});},
						get _cross_refs () {return __get__ (this, function (self, objects) {
							for (var obj of objects.py_values ()) {
								for (var [py_name, value] of objects.py_items ()) {
									setattr (obj, py_name, value);
								}
							}
						});},
						get definition () {return __get__ (this, function (self) {
							return tuple ([self.TRUE, self.FALSE, self.NOT, self.AND, self.OR, self.Symbol]);
						});},
						get symbols () {return __get__ (this, function (self) {
							var args = tuple ([].slice.apply (arguments).slice (1));
							return tuple (map (self.Symbol, args));
						});},
						get parse () {return __get__ (this, function (self, expr, simplify) {
							if (typeof simplify == 'undefined' || (simplify != null && simplify .hasOwnProperty ("__kwargtrans__"))) {;
								var simplify = false;
							};
							var precedence = dict ([[self.NOT, 5], [self.AND, 10], [self.OR, 15], [TOKEN_LPAR, 20]]);
							if (isinstance (expr, str)) {
								var tokenized = self.tokenize (expr);
							}
							else {
								var tokenized = py_iter (expr);
							}
							if (TRACE_PARSE) {
								var tokenized = list (tokenized);
								print ('tokens:');
								map (print, tokenized);
								var tokenized = py_iter (tokenized);
							}
							var ast = list ([null, null]);
							var is_sym = function (_t) {
								return _t == TOKEN_SYMBOL || isinstance (_t, Symbol);
							};
							var prev = null;
							for (var [token, tokstr, position] of tokenized) {
								if (TRACE_PARSE) {
									print ('\nprocessing token:', repr (token), repr (tokstr), repr (position));
								}
								if (prev) {
									var __left0__ = prev;
									var prev_token = __left0__ [0];
									var _ = __left0__ [1];
									var _ = __left0__ [2];
									if (is_sym (prev_token) && is_sym (token)) {
										var __except0__ = ParseError (token, tokstr, position, PARSE_INVALID_SYMBOL_SEQUENCE);
										__except0__.__cause__ = null;
										throw __except0__;
									}
								}
								if (token == TOKEN_SYMBOL) {
									ast.append (self.Symbol (tokstr));
									if (TRACE_PARSE) {
										print (' ast: token == TOKEN_SYMBOL: append new symbol', repr (ast));
									}
								}
								else if (isinstance (token, Symbol)) {
									ast.append (token);
									if (TRACE_PARSE) {
										print (' ast: isinstance(token, Symbol): append existing symbol', repr (ast));
									}
								}
								else if (token == TOKEN_TRUE) {
									ast.append (self.TRUE);
									if (TRACE_PARSE) {
										print ('ast4:', repr (ast));
									}
								}
								else if (token == TOKEN_FALSE) {
									ast.append (self.FALSE);
									if (TRACE_PARSE) {
										print ('ast5:', repr (ast));
									}
								}
								else if (token == TOKEN_NOT) {
									var ast = list ([ast, self.NOT]);
									if (TRACE_PARSE) {
										print ('ast6:', repr (ast));
									}
								}
								else if (token == TOKEN_AND) {
									var ast = self._start_operation (ast, self.AND, precedence);
									if (TRACE_PARSE) {
										print (' ast: token == TOKEN_AND: start_operation', repr (ast));
									}
								}
								else if (token == TOKEN_OR) {
									var ast = self._start_operation (ast, self.OR, precedence);
									if (TRACE_PARSE) {
										print (' ast: token == TOKEN_OR: start_operation', repr (ast));
									}
								}
								else if (token == TOKEN_LPAR) {
									if (prev) {
										var __left0__ = prev;
										var ptoktype = __left0__ [0];
										var _ptokstr = __left0__ [1];
										var _pposition = __left0__ [2];
										if (!__in__ (ptoktype, tuple ([TOKEN_NOT, TOKEN_AND, TOKEN_OR, TOKEN_LPAR]))) {
											var __except0__ = ParseError (token, tokstr, position, PARSE_INVALID_NESTING);
											__except0__.__cause__ = null;
											throw __except0__;
										}
									}
									var ast = list ([ast, TOKEN_LPAR]);
								}
								else if (token == TOKEN_RPAR) {
									while (true) {
										if (ast [0] === null) {
											var __except0__ = ParseError (token, tokstr, position, PARSE_UNBALANCED_CLOSING_PARENS);
											__except0__.__cause__ = null;
											throw __except0__;
										}
										if (ast [1] === TOKEN_LPAR) {
											ast [0].append (ast [2]);
											if (TRACE_PARSE) {
												print ('ast9:', repr (ast));
											}
											var ast = ast [0];
											if (TRACE_PARSE) {
												print ('ast10:', repr (ast));
											}
											break;
										}
										if (isinstance (ast [1], int)) {
											var __except0__ = ParseError (token, tokstr, position, PARSE_UNBALANCED_CLOSING_PARENS);
											__except0__.__cause__ = null;
											throw __except0__;
										}
										if (!(inspect.isclass (ast [1]) && issubclass (ast [1], Function))) {
											var __except0__ = ParseError (token, tokstr, position, PARSE_INVALID_NESTING);
											__except0__.__cause__ = null;
											throw __except0__;
										}
										var subex = ast [1] (...ast.__getslice__ (2, null, 1));
										ast [0].append (subex);
										if (TRACE_PARSE) {
											print ('ast11:', repr (ast));
										}
										var ast = ast [0];
										if (TRACE_PARSE) {
											print ('ast12:', repr (ast));
										}
									}
								}
								else {
									var __except0__ = ParseError (token, tokstr, position, PARSE_UNKNOWN_TOKEN);
									__except0__.__cause__ = null;
									throw __except0__;
								}
								var prev = tuple ([token, tokstr, position]);
							}
							try {
								while (true) {
									if (ast [0] === null) {
										if (ast [1] === null) {
											if (len (ast) != 3) {
												var __except0__ = ParseError (__kwargtrans__ ({error_code: PARSE_INVALID_EXPRESSION}));
												__except0__.__cause__ = null;
												throw __except0__;
											}
											var parsed = ast [2];
											if (TRACE_PARSE) {
												print ('parsed1:', repr (parsed));
											}
										}
										else {
											var parsed = ast [1] (...ast.__getslice__ (2, null, 1));
											if (TRACE_PARSE) {
												print ('parsed2:', repr (parsed));
											}
										}
										break;
									}
									else {
										var subex = ast [1] (...ast.__getslice__ (2, null, 1));
										ast [0].append (subex);
										if (TRACE_PARSE) {
											print ('ast13:', repr (ast));
										}
										var ast = ast [0];
										if (TRACE_PARSE) {
											print ('ast14:', repr (ast));
										}
									}
								}
							}
							catch (__except0__) {
								if (isinstance (__except0__, py_TypeError)) {
									var __except1__ = ParseError (__kwargtrans__ ({error_code: PARSE_INVALID_EXPRESSION}));
									__except1__.__cause__ = null;
									throw __except1__;
								}
								else {
									throw __except0__;
								}
							}
							if (TRACE_PARSE) {
								print ('parsed3:', repr (parsed));
							}
							if (simplify) {
								return parsed.simplify ();
							}
							return parsed;
						});},
						get _start_operation () {return __get__ (this, function (self, ast, operation, precedence) {
							if (TRACE_PARSE) {
								print ('   start_operation: ast, operation, precedence', repr (ast), repr (operation), repr (precedence));
							}
							var op_prec = precedence [operation];
							while (true) {
								if (ast [1] === null) {
									if (TRACE_PARSE) {
										print ('       start_op: ast[1] is None:', repr (ast));
									}
									ast [1] = operation;
									if (TRACE_PARSE) {
										print ('       --> start_op: ast[1] is None:', repr (ast));
									}
									return ast;
								}
								var prec = precedence [ast [1]];
								if (prec > op_prec) {
									if (TRACE_PARSE) {
										print ('       start_op: prec > op_prec:', repr (ast));
									}
									var ast = list ([ast, operation, ast.py_pop (-(1))]);
									if (TRACE_PARSE) {
										print ('       --> start_op: prec > op_prec:', repr (ast));
									}
									return ast;
								}
								if (prec == op_prec) {
									if (TRACE_PARSE) {
										print ('       start_op: prec == op_prec:', repr (ast));
									}
									return ast;
								}
								if (!(inspect.isclass (ast [1]) && issubclass (ast [1], Function))) {
									var __except0__ = ParseError (__kwargtrans__ ({error_code: PARSE_INVALID_NESTING}));
									__except0__.__cause__ = null;
									throw __except0__;
								}
								if (ast [0] === null) {
									if (TRACE_PARSE) {
										print ('       start_op: ast[0] is None:', repr (ast));
									}
									var subexp = ast [1] (...ast.__getslice__ (2, null, 1));
									var new_ast = list ([ast [0], operation, subexp]);
									if (TRACE_PARSE) {
										print ('       --> start_op: ast[0] is None:', repr (new_ast));
									}
									return new_ast;
								}
								else {
									if (TRACE_PARSE) {
										print ('       start_op: else:', repr (ast));
									}
									ast [0].append (ast [1] (...ast.__getslice__ (2, null, 1)));
									var ast = ast [0];
									if (TRACE_PARSE) {
										print ('       --> start_op: else:', repr (ast));
									}
								}
							}
						});},
						get tokenize () {return __get__ (this, function* (self, expr) {
							if (!(isinstance (expr, str))) {
								var __except0__ = py_TypeError (__mod__ ('expr must be string but it is %s.', py_typeof (expr)));
								__except0__.__cause__ = null;
								throw __except0__;
							}
							var TOKENS = dict ({'*': TOKEN_AND, '&': TOKEN_AND, 'and': TOKEN_AND, '+': TOKEN_OR, '|': TOKEN_OR, 'or': TOKEN_OR, '~': TOKEN_NOT, '!': TOKEN_NOT, 'not': TOKEN_NOT, '(': TOKEN_LPAR, ')': TOKEN_RPAR, '[': TOKEN_LPAR, ']': TOKEN_RPAR, 'true': TOKEN_TRUE, '1': TOKEN_TRUE, 'false': TOKEN_FALSE, '0': TOKEN_FALSE, 'none': TOKEN_FALSE});
							var __left0__ = tuple ([0, len (expr)]);
							var position = __left0__ [0];
							var length = __left0__ [1];
							while (position < length) {
								var tok = expr [position];
								var sym = tok.isalpha () || tok == '_';
								if (sym) {
									position++;
									while (position < length) {
										var char = expr [position];
										if (char.isalnum () || __in__ (char, tuple (['.', ':', '_']))) {
											position++;
											tok += char;
										}
										else {
											break;
										}
									}
									position--;
								}
								try {
									yield tuple ([TOKENS [tok.lower ()], tok, position]);
								}
								catch (__except0__) {
									if (isinstance (__except0__, KeyError)) {
										if (sym) {
											yield tuple ([TOKEN_SYMBOL, tok, position]);
										}
										else if (!__in__ (tok, tuple ([' ', '\t', '\r', '\n']))) {
											var __except1__ = ParseError (__kwargtrans__ ({token_string: tok, position: position, error_code: PARSE_UNKNOWN_TOKEN}));
											__except1__.__cause__ = null;
											throw __except1__;
										}
									}
									else {
										throw __except0__;
									}
								}
								position++;
							}
						});},
						get _rdistributive () {return __get__ (this, function (self, expr, op_example) {
							if (expr.isliteral) {
								return expr;
							}
							var expr_class = expr.__class__;
							var args = function () {
								var __accu0__ = [];
								for (var arg of expr.args) {
									__accu0__.append (self._rdistributive (arg, op_example));
								}
								return py_iter (__accu0__);
							} ();
							var args = tuple (function () {
								var __accu0__ = [];
								for (var arg of args) {
									__accu0__.append (arg.simplify ());
								}
								return py_iter (__accu0__);
							} ());
							if (len (args) == 1) {
								return args [0];
							}
							var expr = expr_class (...args);
							var dualoperation = op_example.dual;
							if (isinstance (expr, dualoperation)) {
								var expr = expr.distributive ();
							}
							return expr;
						});},
						get normalize () {return __get__ (this, function (self, expr, operation) {
							var expr = expr.literalize ();
							var expr = expr.simplify ();
							var operation_example = operation (self.TRUE, self.FALSE);
							var expr = self._rdistributive (expr, operation_example);
							var expr = expr.simplify ();
							return expr;
						});},
						get cnf () {return __get__ (this, function (self, expr) {
							return self.normalize (expr, self.AND);
						});},
						get dnf () {return __get__ (this, function (self, expr) {
							return self.normalize (expr, self.OR);
						});}
					});
					var Expression = __class__ ('Expression', [object], {
						sort_order: null,
						args: tuple (),
						isliteral: false,
						iscanonical: false,
						TRUE: null,
						FALSE: null,
						NOT: null,
						AND: null,
						OR: null,
						Symbol: null,
						get objects () {return __get__ (this, function (self) {
							return set (function () {
								var __accu0__ = [];
								for (var s of self.symbols) {
									__accu0__.append (s.obj);
								}
								return py_iter (__accu0__);
							} ());
						});},
						get get_literals () {return __get__ (this, function (self) {
							if (self.isliteral) {
								return list ([self]);
							}
							if (!(self.args)) {
								return list ([]);
							}
							return list (itertools.chain.from_iterable (function () {
								var __accu0__ = [];
								for (var arg of self.args) {
									__accu0__.append (arg.get_literals ());
								}
								return py_iter (__accu0__);
							} ()));
						});},
						get literals () {return __get__ (this, function (self) {
							return set (self.get_literals ());
						});},
						get literalize () {return __get__ (this, function (self) {
							if (self.isliteral) {
								return self;
							}
							var args = tuple (function () {
								var __accu0__ = [];
								for (var arg of self.args) {
									__accu0__.append (arg.literalize ());
								}
								return py_iter (__accu0__);
							} ());
							if (all (function () {
								var __accu0__ = [];
								for (var [i, arg] of enumerate (args)) {
									__accu0__.append (arg === self.args [i]);
								}
								return py_iter (__accu0__);
							} ())) {
								return self;
							}
							return self.__class__ (...args);
						});},
						get get_symbols () {return __get__ (this, function (self) {
							return function () {
								var __accu0__ = [];
								for (var s of self.get_literals ()) {
									__accu0__.append ((isinstance (s, Symbol) ? s : s.args [0]));
								}
								return __accu0__;
							} ();
						});},
						get symbols () {return __get__ (this, function (self) {
							return set (self.get_symbols ());
						});},
						get subs () {return __get__ (this, function (self, substitutions, py_default, simplify) {
							if (typeof py_default == 'undefined' || (py_default != null && py_default .hasOwnProperty ("__kwargtrans__"))) {;
								var py_default = null;
							};
							if (typeof simplify == 'undefined' || (simplify != null && simplify .hasOwnProperty ("__kwargtrans__"))) {;
								var simplify = false;
							};
							for (var [expr, substitution] of substitutions.py_items ()) {
								if (expr == self) {
									return substitution;
								}
							}
							var expr = self._subs (substitutions, py_default, simplify);
							return (expr === null ? self : expr);
						});},
						get _subs () {return __get__ (this, function (self, substitutions, py_default, simplify) {
							var new_arguments = list ([]);
							var changed_something = false;
							if (self === self.TRUE || self === self.FALSE) {
								return self;
							}
							if (!(self.args)) {
								return py_default;
							}
							for (var arg of self.args) {
								var __break1__ = false;
								for (var [expr, substitution] of substitutions.py_items ()) {
									if (arg == expr) {
										new_arguments.append (substitution);
										var changed_something = true;
										__break1__ = true;
										break;
									}
								}
								if (!__break1__) {
									var new_arg = arg._subs (substitutions, py_default, simplify);
									if (new_arg === null) {
										new_arguments.append (arg);
									}
									else {
										new_arguments.append (new_arg);
										var changed_something = true;
									}
								}
							}
							if (!(changed_something)) {
								return ;
							}
							var newexpr = self.__class__ (...new_arguments);
							return (simplify ? newexpr.simplify () : newexpr);
						});},
						get simplify () {return __get__ (this, function (self) {
							return self;
						});},
						get __hash__ () {return __get__ (this, function (self) {
							if (!(self.args)) {
								var arghash = id (self);
							}
							else {
								var arghash = hash (frozenset (map (hash, self.args)));
							}
							return hash (self.__class__.__name__) ^ arghash;
						});},
						get __eq__ () {return __get__ (this, function (self, other) {
							if (self === other) {
								return true;
							}
							if (isinstance (other, self.__class__)) {
								return frozenset (self.args) == frozenset (other.args);
							}
							return NotImplemented;
						});},
						get __ne__ () {return __get__ (this, function (self, other) {
							return !(self == other);
						});},
						get __lt__ () {return __get__ (this, function (self, other) {
							if (self.sort_order !== null && other.sort_order !== null) {
								if (self.sort_order == other.sort_order) {
									return NotImplemented;
								}
								return self.sort_order < other.sort_order;
							}
							return NotImplemented;
						});},
						get __gt__ () {return __get__ (this, function (self, other) {
							var lt = other.__lt__ (self);
							if (lt === NotImplemented) {
								return !(self.__lt__ (other));
							}
							return lt;
						});},
						get __and__ () {return __get__ (this, function (self, other) {
							return self.AND (self, other);
						});},
						get __mul__ () {return __get__ (this, function (self, other) {
							return self.__and__ (other);
						});},
						get __invert__ () {return __get__ (this, function (self) {
							return self.NOT (self);
						});},
						get __or__ () {return __get__ (this, function (self, other) {
							return self.OR (self, other);
						});},
						get __add__ () {return __get__ (this, function (self, other) {
							return self.__or__ (other);
						});},
						get __bool__ () {return __get__ (this, function (self) {
							var __except0__ = py_TypeError ('Cannot evaluate expression as a Python Boolean.');
							__except0__.__cause__ = null;
							throw __except0__;
						});},
						get __nonzero__ () {return __get__ (this, function (self) {
							return self.__bool__ ();
						});}
					});
					var BaseElement = __class__ ('BaseElement', [Expression], {
						sort_order: 0,
						get __init__ () {return __get__ (this, function (self) {
							__super__ (BaseElement, '__init__') (self);
							self.iscanonical = true;
							self.dual = null;
						});},
						get __lt__ () {return __get__ (this, function (self, other) {
							if (isinstance (other, BaseElement)) {
								return self == self.FALSE;
							}
							return NotImplemented;
						});},
						get __bool__ () {return __get__ (this, function (self) {
							return null;
						});},
						get __nonzero__ () {return __get__ (this, function (self) {
							return null;
						});},
						get pretty () {return __get__ (this, function (self, indent, debug) {
							if (typeof indent == 'undefined' || (indent != null && indent .hasOwnProperty ("__kwargtrans__"))) {;
								var indent = 0;
							};
							if (typeof debug == 'undefined' || (debug != null && debug .hasOwnProperty ("__kwargtrans__"))) {;
								var debug = false;
							};
							return ' ' * indent + repr (self);
						});}
					});
					var _TRUE = __class__ ('_TRUE', [BaseElement], {
						get __init__ () {return __get__ (this, function (self) {
							__super__ (_TRUE, '__init__') (self);
						});},
						get __hash__ () {return __get__ (this, function (self) {
							return hash (true);
						});},
						get __eq__ () {return __get__ (this, function (self, other) {
							return self === other || other === true || isinstance (other, _TRUE);
						});},
						get __str__ () {return __get__ (this, function (self) {
							return '1';
						});},
						get __repr__ () {return __get__ (this, function (self) {
							return 'TRUE';
						});},
						get __bool__ () {return __get__ (this, function (self) {
							return true;
						});},
						get __nonzero__ () {return __get__ (this, function (self) {
							return true;
						});}
					});
					var _FALSE = __class__ ('_FALSE', [BaseElement], {
						get __init__ () {return __get__ (this, function (self) {
							__super__ (_FALSE, '__init__') (self);
						});},
						get __hash__ () {return __get__ (this, function (self) {
							return hash (false);
						});},
						get __eq__ () {return __get__ (this, function (self, other) {
							return self === other || other === false || isinstance (other, _FALSE);
						});},
						get __str__ () {return __get__ (this, function (self) {
							return '0';
						});},
						get __repr__ () {return __get__ (this, function (self) {
							return 'FALSE';
						});},
						get __bool__ () {return __get__ (this, function (self) {
							return false;
						});},
						get __nonzero__ () {return __get__ (this, function (self) {
							return false;
						});}
					});
					var Symbol = __class__ ('Symbol', [Expression], {
						sort_order: 5,
						get __init__ () {return __get__ (this, function (self, obj) {
							__super__ (Symbol, '__init__') (self);
							self.obj = obj;
							self.iscanonical = true;
							self.isliteral = true;
						});},
						get __hash__ () {return __get__ (this, function (self) {
							if (self.obj === null) {
								return id (self);
							}
							return hash (self.obj);
						});},
						get __eq__ () {return __get__ (this, function (self, other) {
							if (self === other) {
								return true;
							}
							if (isinstance (other, self.__class__)) {
								return self.obj == other.obj;
							}
							return NotImplemented;
						});},
						get __lt__ () {return __get__ (this, function (self, other) {
							var comparator = Expression.__lt__ (self, other);
							if (comparator !== NotImplemented) {
								return comparator;
							}
							if (isinstance (other, Symbol)) {
								return self.obj < other.obj;
							}
							return NotImplemented;
						});},
						get __str__ () {return __get__ (this, function (self) {
							return str (self.obj);
						});},
						get __repr__ () {return __get__ (this, function (self) {
							var obj = (isinstance (self.obj, str) ? __mod__ ("'%s'", self.obj) : repr (self.obj));
							return __mod__ ('%s(%s)', tuple ([self.__class__.__name__, obj]));
						});},
						get pretty () {return __get__ (this, function (self, indent, debug) {
							if (typeof indent == 'undefined' || (indent != null && indent .hasOwnProperty ("__kwargtrans__"))) {;
								var indent = 0;
							};
							if (typeof debug == 'undefined' || (debug != null && debug .hasOwnProperty ("__kwargtrans__"))) {;
								var debug = false;
							};
							var debug_details = '';
							if (debug) {
								debug_details += __mod__ ('<isliteral=%r, iscanonical=%r>', tuple ([self.isliteral, self.iscanonical]));
							}
							var obj = (isinstance (self.obj, str) ? __mod__ ("'%s'", self.obj) : repr (self.obj));
							return ' ' * indent + __mod__ ('%s(%s%s)', tuple ([self.__class__.__name__, debug_details, obj]));
						});}
					});
					var Function = __class__ ('Function', [Expression], {
						get __init__ () {return __get__ (this, function (self) {
							var args = tuple ([].slice.apply (arguments).slice (1));
							__super__ (Function, '__init__') (self);
							self.operator = null;
							self.args = tuple (args);
						});},
						get __str__ () {return __get__ (this, function (self) {
							var args = self.args;
							if (len (args) == 1) {
								if (self.isliteral) {
									return __mod__ ('%s%s', tuple ([self.operator, args [0]]));
								}
								return __mod__ ('%s(%s)', tuple ([self.operator, args [0]]));
							}
							var args_str = list ([]);
							for (var arg of args) {
								if (arg.isliteral) {
									args_str.append (str (arg));
								}
								else {
									args_str.append (__mod__ ('(%s)', arg));
								}
							}
							return self.operator.join (args_str);
						});},
						get __repr__ () {return __get__ (this, function (self) {
							return __mod__ ('%s(%s)', tuple ([self.__class__.__name__, ', '.join (map (repr, self.args))]));
						});},
						get pretty () {return __get__ (this, function (self, indent, debug) {
							if (typeof indent == 'undefined' || (indent != null && indent .hasOwnProperty ("__kwargtrans__"))) {;
								var indent = 0;
							};
							if (typeof debug == 'undefined' || (debug != null && debug .hasOwnProperty ("__kwargtrans__"))) {;
								var debug = false;
							};
							var debug_details = '';
							if (debug) {
								debug_details += __mod__ ('<isliteral=%r, iscanonical=%r', tuple ([self.isliteral, self.iscanonical]));
								var identity = getattr (self, 'identity', null);
								if (identity !== null) {
									debug_details += __mod__ (', identity=%r', identity);
								}
								var annihilator = getattr (self, 'annihilator', null);
								if (annihilator !== null) {
									debug_details += __mod__ (', annihilator=%r', annihilator);
								}
								var dual = getattr (self, 'dual', null);
								if (dual !== null) {
									debug_details += __mod__ (', dual=%r', dual);
								}
								debug_details += '>';
							}
							var cls = self.__class__.__name__;
							var args = function () {
								var __accu0__ = [];
								for (var a of self.args) {
									__accu0__.append (a.pretty (__kwargtrans__ ({indent: indent + 2, debug: debug})));
								}
								return __accu0__;
							} ();
							var pfargs = ',\n'.join (args);
							var cur_indent = ' ' * indent;
							var new_line = (self.isliteral ? '' : '\n');
							return '{cur_indent}{cls}({debug_details}{new_line}{pfargs}\n{cur_indent})'.format (__kwargtrans__ (locals ()));
						});}
					});
					var NOT = __class__ ('NOT', [Function], {
						get __init__ () {return __get__ (this, function (self, arg1) {
							__super__ (NOT, '__init__') (self, arg1);
							self.isliteral = isinstance (self.args [0], Symbol);
							self.operator = '~';
						});},
						get literalize () {return __get__ (this, function (self) {
							var expr = self.demorgan ();
							if (isinstance (expr, self.__class__)) {
								return expr;
							}
							return expr.literalize ();
						});},
						get simplify () {return __get__ (this, function (self) {
							if (self.iscanonical) {
								return self;
							}
							var expr = self.cancel ();
							if (!(isinstance (expr, self.__class__))) {
								return expr.simplify ();
							}
							if (__in__ (expr.args [0], tuple ([self.TRUE, self.FALSE]))) {
								return expr.args [0].dual;
							}
							var expr = self.__class__ (expr.args [0].simplify ());
							expr.iscanonical = true;
							return expr;
						});},
						get cancel () {return __get__ (this, function (self) {
							var expr = self;
							while (true) {
								var arg = expr.args [0];
								if (!(isinstance (arg, self.__class__))) {
									return expr;
								}
								var expr = arg.args [0];
								if (!(isinstance (expr, self.__class__))) {
									return expr;
								}
							}
						});},
						get demorgan () {return __get__ (this, function (self) {
							var expr = self.cancel ();
							if (expr.isliteral || !(isinstance (expr.args [0], tuple ([self.NOT, self.AND, self.OR])))) {
								return expr;
							}
							var op = expr.args [0];
							return op.dual (...function () {
								var __accu0__ = [];
								for (var arg of op.args) {
									__accu0__.append (self.__class__ (arg).cancel ());
								}
								return py_iter (__accu0__);
							} ());
						});},
						get __lt__ () {return __get__ (this, function (self, other) {
							return self.args [0] < other;
						});},
						get pretty () {return __get__ (this, function (self, indent, debug) {
							if (typeof indent == 'undefined' || (indent != null && indent .hasOwnProperty ("__kwargtrans__"))) {;
								var indent = 1;
							};
							if (typeof debug == 'undefined' || (debug != null && debug .hasOwnProperty ("__kwargtrans__"))) {;
								var debug = false;
							};
							var debug_details = '';
							if (debug) {
								debug_details += __mod__ ('<isliteral=%r, iscanonical=%r>', tuple ([self.isliteral, self.iscanonical]));
							}
							if (self.isliteral) {
								var pretty_literal = self.args [0].pretty (__kwargtrans__ ({indent: 0, debug: debug}));
								return ' ' * indent + __mod__ ('%s(%s%s)', tuple ([self.__class__.__name__, debug_details, pretty_literal]));
							}
							else {
								return __super__ (NOT, 'pretty') (self, __kwargtrans__ ({indent: indent, debug: debug}));
							}
						});}
					});
					var DualBase = __class__ ('DualBase', [Function], {
						get __init__ () {return __get__ (this, function (self, arg1, arg2) {
							var args = tuple ([].slice.apply (arguments).slice (3));
							__super__ (DualBase, '__init__') (self, arg1, arg2, ...args);
							self.identity = null;
							self.annihilator = null;
							self.dual = null;
						});},
						get __contains__ () {return __get__ (this, function (self, expr) {
							if (__in__ (expr, self.args)) {
								return true;
							}
							if (isinstance (expr, self.__class__)) {
								return all (function () {
									var __accu0__ = [];
									for (var arg of expr.args) {
										__accu0__.append (__in__ (arg, self.args));
									}
									return py_iter (__accu0__);
								} ());
							}
						});},
						get simplify () {return __get__ (this, function (self) {
							if (self.iscanonical) {
								return self;
							}
							var args = function () {
								var __accu0__ = [];
								for (var arg of self.args) {
									__accu0__.append (arg.simplify ());
								}
								return __accu0__;
							} ();
							var expr = self.__class__ (...args);
							var expr = expr.literalize ();
							var expr = expr.flatten ();
							if (__in__ (self.annihilator, expr.args)) {
								return self.annihilator;
							}
							var args = list ([]);
							for (var arg of expr.args) {
								if (!__in__ (arg, args)) {
									args.append (arg);
								}
							}
							if (len (args) == 1) {
								return args [0];
							}
							if (__in__ (self.identity, args)) {
								args.remove (self.identity);
								if (len (args) == 1) {
									return args [0];
								}
							}
							for (var arg of args) {
								if (__in__ (self.NOT (arg), args)) {
									return self.annihilator;
								}
							}
							var i = 0;
							while (i < len (args) - 1) {
								var j = i + 1;
								var ai = args [i];
								if (!(isinstance (ai, self.dual))) {
									i++;
									continue;
								}
								while (j < len (args)) {
									var aj = args [j];
									if (!(isinstance (aj, self.dual)) || len (ai.args) != len (aj.args)) {
										j++;
										continue;
									}
									var negated = null;
									for (var arg of ai.args) {
										if (__in__ (arg, aj.args)) {
											// pass;
										}
										else if (__in__ (self.NOT (arg).cancel (), aj.args)) {
											if (negated === null) {
												var negated = arg;
											}
											else {
												var negated = null;
												break;
											}
										}
										else {
											var negated = null;
											break;
										}
									}
									if (negated !== null) {
										delete args [j];
										var aiargs = list (ai.args);
										aiargs.remove (negated);
										if (len (aiargs) == 1) {
											args [i] = aiargs [0];
										}
										else {
											args [i] = self.dual (...aiargs);
										}
										if (len (args) == 1) {
											return args [0];
										}
										else {
											return self.__class__ (...args).simplify ();
										}
									}
									j++;
								}
								i++;
							}
							var args = self.absorb (args);
							if (len (args) == 1) {
								return args [0];
							}
							args.py_sort ();
							var expr = self.__class__ (...args);
							expr.iscanonical = true;
							return expr;
						});},
						get flatten () {return __get__ (this, function (self) {
							var args = list (self.args);
							var i = 0;
							for (var arg of self.args) {
								if (isinstance (arg, self.__class__)) {
									args.__setslice__ (i, i + 1, null, arg.args);
									i += len (arg.args);
								}
								else {
									i++;
								}
							}
							return self.__class__ (...args);
						});},
						get absorb () {return __get__ (this, function (self, args) {
							var args = list (args);
							if (!(args)) {
								var args = list (self.args);
							}
							var i = 0;
							while (i < len (args)) {
								var absorber = args [i];
								var j = 0;
								while (j < len (args)) {
									if (j == i) {
										j++;
										continue;
									}
									var target = args [j];
									if (!(isinstance (target, self.dual))) {
										j++;
										continue;
									}
									if (__in__ (absorber, target)) {
										delete args [j];
										if (j < i) {
											i--;
										}
										continue;
									}
									var neg_absorber = self.NOT (absorber).cancel ();
									if (__in__ (neg_absorber, target)) {
										var b = target.subtract (neg_absorber, __kwargtrans__ ({simplify: false}));
										if (b === null) {
											delete args [j];
											if (j < i) {
												i--;
											}
											continue;
										}
										else {
											args [j] = b;
											j++;
											continue;
										}
									}
									if (isinstance (absorber, self.dual)) {
										var remove = null;
										for (var arg of absorber.args) {
											var narg = self.NOT (arg).cancel ();
											if (__in__ (arg, target.args)) {
												// pass;
											}
											else if (__in__ (narg, target.args)) {
												if (remove === null) {
													var remove = narg;
												}
												else {
													var remove = null;
													break;
												}
											}
											else {
												var remove = null;
												break;
											}
										}
										if (remove !== null) {
											args [j] = target.subtract (remove, __kwargtrans__ ({simplify: true}));
										}
									}
									j++;
								}
								i++;
							}
							return args;
						});},
						get subtract () {return __get__ (this, function (self, expr, simplify) {
							var args = self.args;
							if (__in__ (expr, self.args)) {
								var args = list (self.args);
								args.remove (expr);
							}
							else if (isinstance (expr, self.__class__)) {
								if (all (function () {
									var __accu0__ = [];
									for (var arg of expr.args) {
										__accu0__.append (__in__ (arg, self.args));
									}
									return py_iter (__accu0__);
								} ())) {
									var args = tuple (function () {
										var __accu0__ = [];
										for (var arg of self.args) {
											if (!__in__ (arg, expr)) {
												__accu0__.append (arg);
											}
										}
										return py_iter (__accu0__);
									} ());
								}
							}
							if (len (args) == 0) {
								return null;
							}
							if (len (args) == 1) {
								return args [0];
							}
							var newexpr = self.__class__ (...args);
							if (simplify) {
								var newexpr = newexpr.simplify ();
							}
							return newexpr;
						});},
						get distributive () {return __get__ (this, function (self) {
							var dual = self.dual;
							var args = list (self.args);
							for (var [i, arg] of enumerate (args)) {
								if (isinstance (arg, dual)) {
									args [i] = arg.args;
								}
								else {
									args [i] = tuple ([arg]);
								}
							}
							var prod = itertools.product (...args);
							var args = tuple (function () {
								var __accu0__ = [];
								for (var arg of prod) {
									__accu0__.append (self.__class__ (...arg).simplify ());
								}
								return py_iter (__accu0__);
							} ());
							if (len (args) == 1) {
								return args [0];
							}
							else {
								return dual (...args);
							}
						});},
						get __lt__ () {return __get__ (this, function (self, other) {
							var comparator = Expression.__lt__ (self, other);
							if (comparator !== NotImplemented) {
								return comparator;
							}
							if (isinstance (other, self.__class__)) {
								var lenself = len (self.args);
								var lenother = len (other.args);
								for (var i = 0; i < min (lenself, lenother); i++) {
									if (self.args [i] == other.args [i]) {
										continue;
									}
									var comparator = self.args [i] < other.args [i];
									if (comparator !== NotImplemented) {
										return comparator;
									}
								}
								if (lenself != lenother) {
									return lenself < lenother;
								}
							}
							return NotImplemented;
						});}
					});
					var AND = __class__ ('AND', [DualBase], {
						sort_order: 10,
						get __init__ () {return __get__ (this, function (self, arg1, arg2) {
							var args = tuple ([].slice.apply (arguments).slice (3));
							__super__ (AND, '__init__') (self, arg1, arg2, ...args);
							self.identity = self.TRUE;
							self.annihilator = self.FALSE;
							self.dual = self.OR;
							self.operator = '&';
						});}
					});
					var OR = __class__ ('OR', [DualBase], {
						sort_order: 25,
						get __init__ () {return __get__ (this, function (self, arg1, arg2) {
							var args = tuple ([].slice.apply (arguments).slice (3));
							__super__ (OR, '__init__') (self, arg1, arg2, ...args);
							self.identity = self.FALSE;
							self.annihilator = self.TRUE;
							self.dual = self.AND;
							self.operator = '|';
						});}
					});
					__pragma__ ('<use>' +
						'__future__' +
						'inspect' +
						'itertools' +
					'</use>')
					__pragma__ ('<all>')
						__all__.AND = AND;
						__all__.BaseElement = BaseElement;
						__all__.BooleanAlgebra = BooleanAlgebra;
						__all__.DualBase = DualBase;
						__all__.Expression = Expression;
						__all__.Function = Function;
						__all__.NOT = NOT;
						__all__.OR = OR;
						__all__.PARSE_ERRORS = PARSE_ERRORS;
						__all__.PARSE_INVALID_EXPRESSION = PARSE_INVALID_EXPRESSION;
						__all__.PARSE_INVALID_NESTING = PARSE_INVALID_NESTING;
						__all__.PARSE_INVALID_SYMBOL_SEQUENCE = PARSE_INVALID_SYMBOL_SEQUENCE;
						__all__.PARSE_UNBALANCED_CLOSING_PARENS = PARSE_UNBALANCED_CLOSING_PARENS;
						__all__.PARSE_UNKNOWN_TOKEN = PARSE_UNKNOWN_TOKEN;
						__all__.ParseError = ParseError;
						__all__.Symbol = Symbol;
						__all__.TOKEN_AND = TOKEN_AND;
						__all__.TOKEN_FALSE = TOKEN_FALSE;
						__all__.TOKEN_LPAR = TOKEN_LPAR;
						__all__.TOKEN_NOT = TOKEN_NOT;
						__all__.TOKEN_OR = TOKEN_OR;
						__all__.TOKEN_RPAR = TOKEN_RPAR;
						__all__.TOKEN_SYMBOL = TOKEN_SYMBOL;
						__all__.TOKEN_TRUE = TOKEN_TRUE;
						__all__.TOKEN_TYPES = TOKEN_TYPES;
						__all__.TRACE_PARSE = TRACE_PARSE;
						__all__._FALSE = _FALSE;
						__all__._TRUE = _TRUE;
						__all__.absolute_import = absolute_import;
						__all__.print_function = print_function;
						__all__.unicode_literals = unicode_literals;
					__pragma__ ('</all>')
				}
			}
		}
	);
	__nest__ (
		__all__,
		'inspect', {
			__all__: {
				__inited__: false,
				__init__: function (__all__) {
					var isclass = function (object) {
						return hasattr (object, '__metaclass__') && !(hasattr (object, '__class__'));
					};
					__pragma__ ('<all>')
						__all__.isclass = isclass;
					__pragma__ ('</all>')
				}
			}
		}
	);
    __nest__ (
        __all__,
        'itertools', {
            __all__: {
                __inited__: false,
                __init__: function (__all__) {
                    var chain = function () {
                        var args = [] .slice.apply (arguments);
                        var result = [];
                        for (var index = 0; index < args.length; index++) {
                            result = result.concat (args [index]);
                        }
                        return list (result);
                    }
                    //<all>
                    __all__.chain = chain;
                    //</all>
                }
            }
        }
    );
	(function () {
		var absolute_import = __init__ (__world__.__future__).absolute_import;
		var unicode_literals = __init__ (__world__.__future__).unicode_literals;
		var print_function = __init__ (__world__.__future__).print_function;
		var BooleanAlgebra = __init__ (__world__.boolean.boolean).BooleanAlgebra;
		var Expression = __init__ (__world__.boolean.boolean).Expression;
		var Symbol = __init__ (__world__.boolean.boolean).Symbol;
		var ParseError = __init__ (__world__.boolean.boolean).ParseError;
		var PARSE_ERRORS = __init__ (__world__.boolean.boolean).PARSE_ERRORS;
		var AND = __init__ (__world__.boolean.boolean).AND;
		var NOT = __init__ (__world__.boolean.boolean).NOT;
		var OR = __init__ (__world__.boolean.boolean).OR;
		var TOKEN_TRUE = __init__ (__world__.boolean.boolean).TOKEN_TRUE;
		var TOKEN_FALSE = __init__ (__world__.boolean.boolean).TOKEN_FALSE;
		var TOKEN_SYMBOL = __init__ (__world__.boolean.boolean).TOKEN_SYMBOL;
		var TOKEN_AND = __init__ (__world__.boolean.boolean).TOKEN_AND;
		var TOKEN_OR = __init__ (__world__.boolean.boolean).TOKEN_OR;
		var TOKEN_NOT = __init__ (__world__.boolean.boolean).TOKEN_NOT;
		var TOKEN_LPAR = __init__ (__world__.boolean.boolean).TOKEN_LPAR;
		var TOKEN_RPAR = __init__ (__world__.boolean.boolean).TOKEN_RPAR;
		__pragma__ ('<use>' +
			'__future__' +
			'boolean.boolean' +
		'</use>')
		__pragma__ ('<all>')
			__all__.AND = AND;
			__all__.BooleanAlgebra = BooleanAlgebra;
			__all__.Expression = Expression;
			__all__.NOT = NOT;
			__all__.OR = OR;
			__all__.PARSE_ERRORS = PARSE_ERRORS;
			__all__.ParseError = ParseError;
			__all__.Symbol = Symbol;
			__all__.TOKEN_AND = TOKEN_AND;
			__all__.TOKEN_FALSE = TOKEN_FALSE;
			__all__.TOKEN_LPAR = TOKEN_LPAR;
			__all__.TOKEN_NOT = TOKEN_NOT;
			__all__.TOKEN_OR = TOKEN_OR;
			__all__.TOKEN_RPAR = TOKEN_RPAR;
			__all__.TOKEN_SYMBOL = TOKEN_SYMBOL;
			__all__.TOKEN_TRUE = TOKEN_TRUE;
			__all__.absolute_import = absolute_import;
			__all__.print_function = print_function;
			__all__.unicode_literals = unicode_literals;
		__pragma__ ('</all>')
	}) ();
   return __all__;
}
window ['boolean'] = boolean ();
