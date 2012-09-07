  #define noinline false
  #define doinline true
  
  struct Lambda
  {
    string name;
    string ret_type;
    unsigned long nargs;
    map<string,string> arguments;
    bool fastcc;
    bool tco;
    bool lining;
    string docstring;
  };
  
  struct MetaFunction
  {
    vector<Lambda> versions;
  };
  
  map<string,MetaFunction> FunctionTable;
  map<string,Lambda> Macros;
  
  /*
   * - Iterate over the arguments, noting type-annotated and polymorphic arguments
   * - Iterate over the code, storing all expressions in a list 'precode'
   * - Find a typecase, ensure there is no code after it
   * - Ensure the first argument to typecase is an argument declared as being polymorphic
   * - Iterate over every branch of the typecase, ensuring the first element of each list is a valid type name
   *  - Call defineFunction with the same function name, where the code is the precode plus the rest of the code in this branch (This should recursively apply nested typecases).*/
  
  string fn_code;
  
  void print(map<string,string> in)
  {
    printf("{\n");
    for(map<string,string>::iterator i = in.begin(); i != in.end(); i++)
    {
      printf("\t%s : %s\n", i->first.c_str(), i->second.c_str());
    }
    printf("}\n");
  }
  
  void print_metafunction(string in)
  {
    for(unsigned int i = 0; i < FunctionTable[in].versions.size(); i++)
    {
      printf("\t\t{\n");
      printf("\t\t\t Return type: %s\n",FunctionTable[in].versions[i].ret_type.c_str());
      printf("\t\t\t Number of Arguments: %lu\n",FunctionTable[in].versions[i].nargs);
      printf("\t\t\t Tail Call Optimized?: %s\n",FunctionTable[in].versions[i].tco ? "Yes" : "No");
      printf("\t\t}\n");
    }
  }
  
  void print_fntable()
  {
    printf("{\n");
    for(map<string,MetaFunction>::iterator i = FunctionTable.begin(); i != FunctionTable.end(); i++)
    {
      printf("\t%s: \n",i->first.c_str());
      print_metafunction(i->first);
    }
    printf("}\n");
  }
  
  enum fnType {Function, Fast, Recursive};
  
  #define args (nth(form,args_pos))
  
  const unsigned int name_pos = 1;
  const unsigned int ret_type_pos = 2;
  const unsigned int args_pos = 3;
  const unsigned int docstring_pos = 4;
  
  void validate_function(Form* form)
  {
    if(length(form) < 2)
      { printf("ERROR: No name."); Unwind(); }
    else if(length(form) < 3)
      { printf("ERROR: No return type."); Unwind(); }
    else if(length(form) < 4)
      { printf("ERROR: No argument list."); Unwind(); }
    if(length(form) < 5)
      { printf("ERROR: No body."); Unwind(); }
    if(tag(nth(form,name_pos)) != Atom)
      { printf("ERROR: Can't use a list as a function name."); Unwind(); }
    else if(analyze(val(nth(form,name_pos))) != Symbol)
      { printf("ERROR: Can't use a non-symbol as a function name."); Unwind(); }
    if(args != NULL)
    {
      if(tag(args) != List)
      { printf("ERROR: The argument list must be a list."); Unwind(); }
    }
  }
  
  /*string poly_code;
  
  string polymorphicExpansion(Form* form, unsigned int pos)
  {
    map<string,unsigned long> arguments;
    unsigned long nargs = length(args);
    unsigned int i;
    Form* current;
    if(nargs > 0)
    {
      for(i = 0; i < length(args); i++)
      {
        current = nth(args,i);
        if(islist(current))
        {
          if(isatom(car(current)))
          {
            if(val(car(current)) == "poly")
            {
              arguments[val(cdr(current))] = i;
            }             
          }
        }
      }
      for(;pos < length(form); pos++)
      {
        current = nth(form,pos);
        unsigned long argument_pos;
        string precode;
        string postcode;
        vector<Form> replacements;
        if(islist(current))
        {
          if(isatom(car(current)))
          {
            
            if(val(car(current)) == "typecase")
            {
              string type;
              if(length(current) < 3)
              {
                printf("ERROR: A typecase form takes at least three arguments.");
                Unwind();
              }
              if(islist(nth(current,1)))
              {
                printf("ERROR: The first argument of a typecase must be the argument name");
                Unwind();
              }
              else
              {
                type = val(nth(current,1));
                map<string,unsigned long>::iterator seeker = arguments.find(type);
                if(seeker != arguments.end())
                {
                  //Argument exists
                  argument_pos = seeker->second;
                  //Do the rest
                }
                else
                {
                  printf("ERROR: A non-polymorphic or nonexistent argument was provided to a typecase form.");
                  Unwind();
                }
              }
              //Iterate over the cases
              for(i = 2; i < length(current); i++)
              {
                if(isatom(nth(current,i)))
                {
                  printf("ERROR: An atom was found in a typecase form.");
                  Unwind();
                }
                else
                {
                  if(islist(car(nth(current,i))))
                  {
                    printf("ERROR: The first element in every pair of typecase form must be a symbolic atom.");
                    Unwind();
                  }
                  else if(analyze(preprint(car(nth(current,i)))) == Symbol)
                  {
                    printf("ERROR: The first element in every pair of typecase form must be a symbolic atom.");
                    Unwind();
                  }
                  else
                  {
                    string replacement_type = preprint(car(nth(current,i)));
                    Form* replacement_code = cdr(nth(current,i));
                  }
                }
              }
            }
          }
        }
      }
    }
    else
      return preprint(form);
  }*/
  
  string defineFunction(Form* form, fnType fn_type, bool inlining)
  {
    //0                                                1      2          3             [3]         3/4 (Depending on the docstring)
    //(function|fast|recursive|inline|recursive-inline [name] [ret_type] (arg1, arg1, ..., argn) "docstring" [body]*
    const bool has_docstring = /*((tag(nth(form,docstring_pos)) == Atom) && (analyze(val(nth(form,docstring_pos))) == String))*/ false;
    const unsigned int body_starting_pos = (has_docstring ? 1+docstring_pos : docstring_pos); 
    validate_function(form);
    Lambda newfn;
    if(fn_type == Function) { newfn.fastcc = false; newfn.tco = false; newfn.lining = inlining; }
    else if(fn_type == Fast) { newfn.fastcc = true; newfn.tco = false; newfn.lining = inlining; }
    else if(fn_type == Recursive) { newfn.fastcc = true; newfn.tco = true; newfn.lining = inlining; }
    string fn_name = val(nth(form,name_pos));
    string fn_ret_type = printTypeSignature(nth(form,ret_type_pos));
    newfn.ret_type = fn_ret_type;
    map<string,Lambda>::iterator macfind = Macros.find(fn_name);
    if(macfind != Macros.end())
      { printf("ERROR: A macro has already been defined with that name."); Unwind(); }
    Scope new_scope;
    SymbolTable.push_back(new_scope);
    //Iterate over arguments
    map<string,string> fn_args;
    Form* current_arg;
    if(args != NULL)
    {
      for(long i = 0; i < length(args); i++)
      {
        current_arg = nth(args,i);
        //printf("\nAnalyzing:%s",preprint(current_arg).c_str());
        if(tag(current_arg) == List)
        {
          if(tag(nth(current_arg,0)) != Atom)
          {
            printf("ERROR: An argument name must be a symbol, not a list or any other kind of atom.");
            Unwind();
          }
          else
          {
            string argname = val(nth(current_arg,0));
            map<string,string>::iterator seeker = fn_args.find(argname);
            if(seeker != fn_args.end())
            {
              printf("ERROR: Argument name '%s' was already defined in this argument list:\n%s",
                     argname.c_str(),preprint(nth(form,2)).c_str());
            }
            else
            {
              string type = preprint(nth(current_arg,1));
              fn_args[argname] = type;
              SymbolTable[ScopeDepth][argname] = type;
            }
          }
        }
        else
        { 
          //Handle Dynamic args
        }
      }
    }
    newfn.arguments = fn_args;
    newfn.nargs = ((unsigned)fn_args.size())/2;
    string tmp_code;
    map<string,MetaFunction>::iterator seeker = FunctionTable.find(fn_name);
    if(seeker != FunctionTable.end())
    {
      for(unsigned int i = 0; i < seeker->second.versions.size(); i++)
      {
        if(newfn.nargs == 0 && seeker->second.versions[i].nargs == 0)
        {
          printf("ERROR: A function with the same prototype has already been defined.");
          Unwind();
        }
        else if(fn_args == seeker->second.versions[i].arguments /*&& (fn_ret_type == seeker->second.versions[i].ret_type)*/) //Functions may not yet be differentiated by return type
        {
          printf("ERROR: A function with the same prototype has already been defined.");
          Unwind();
        }
      }
      seeker->second.versions.push_back(newfn);
      string arg_code;
      for(map<string,string>::iterator i = fn_args.begin(); i != fn_args.end(); i++)
      {
        arg_code += i->second + " %" + i->first+to_string(ScopeDepth) + ", ";
      }
      tmp_code = "define " + fn_type + (string)" @" + fn_name + to_string(seeker->second.versions.size()-1)
      + "(" + cutlast(arg_code) + "){\n";
    }
    else
    {
      MetaFunction new_metafn;
      new_metafn.versions.push_back(newfn);
      FunctionTable[fn_name] = new_metafn;
      string arg_code;
      //print(fn_args);
      for(map<string,string>::iterator i = fn_args.begin(); i != fn_args.end(); i++)
      {
        arg_code += i->second + " %" + i->first+to_string(ScopeDepth) + ", ";
      }
      tmp_code = "define " + fn_type + (string)" @" + fn_name + "0" + "(" + cutlast(arg_code)
      + "){\n";
    }
    //Compile the code
    for(unsigned long i = body_starting_pos; i < length(form);i++)
    {
      tmp_code += emitCode(nth(form,i));
    }
    fn_code = tmp_code + "ret " + latest_type() + " " + get_current_res() + "\n}";;
    return fn_code;
  }
  
  string callGeneric(long gen_pos, Form* code)
  {
    printf("Not implemented yet.");
    Unwind();
  }
  
  string callFunction(string func, Form* code)
  {
    if(analyze(func) != Symbol)
    {
      printf("ERROR: A non-symbolic atom was used as a function name.");
      Unwind();
    }
    else
    {
      for(unsigned long i = 0; i < Generics.size(); i++)
      {
        if(Generics[i].first == func)
        {
          if(Generics[i].second.id == typeFunction)
          {
            return callGeneric(i,code);
          }
        }
      }
      map<string,MetaFunction>::iterator seeker = FunctionTable.find(func);
      if(seeker != FunctionTable.end())
      {
        vector<string> arguments;
        vector<long long int> res_nums;
        string callcode;
        if(code != NULL)
        {
          Form* curr;
          for(long i = 0; i < length(code); i++)
          {
            curr = nth(code,i);
            callcode += emitCode(curr);
            arguments.push_back(res_type(get_current_res()));
            res_nums.push_back(res_version);
          }
        }
        for(unsigned int i = 0; i < seeker->second.versions.size(); i++)
        {
          if(seeker->second.versions[i].arguments.size() == arguments.size())
          {
            map<string,string>::iterator arg_iterator = seeker->second.versions[i].arguments.begin();
            for(unsigned int j = 0; j < arguments.size(); j++)
            {
              if(arguments[j] != arg_iterator->second)
              {
                break;
              }
              //We don't have to check that the iterator is != arguments.end()
              //because that is done implicitly by the size() == arguments.size() check
              arg_iterator++;
            }
            //Found a matching function
            callcode += get_unique_res(seeker->second.versions[i].ret_type);
            callcode += " = " + (string)(seeker->second.versions[i].tco ? "tail call " : "call ");
            callcode += seeker->second.versions[i].fastcc ? "fastcc " : "ccc ";
            callcode += seeker->second.versions[i].ret_type + " @" + func + to_string(i) + (arguments.empty() ? "" :"(");
            for(arg_iterator = seeker->second.versions[i].arguments.begin();
                arg_iterator != seeker->second.versions[i].arguments.end();
            arg_iterator++)
                {
                  callcode += arg_iterator->second + " ";
                  callcode += get_res(res_nums[0]) + ", ";
                  res_nums.erase(res_nums.begin());
                }
                callcode = (arguments.empty() ? callcode : (cutlast(callcode) + ")"));
                return callcode;
          }
        }
        printf("ERROR: Couldn't find a matching function.");
        Unwind();
      }
      else
      {
        printf("ERROR: Couldn't find a matching function.");
        Unwind();
      }        
    }
  }
  
  /*string callFunction(string func, Form* form)
  {
    printf("Calling the function '%s'.\n", func.c_str());
    map<string,MetaFunction>::iterator find_fn = FunctionTable.find(func);
    if(find_fn != FunctionTable.end())
    {
      printf("All good 0\n");
      string tmp_code;
      unsigned long nargs = length(form)-1;
      vector<string> types;
      for(unsigned long i = 1; i < nargs; i++)
      {
        tmp_code += emitCode(nth(form,i));
        types.push_back(latest_type());
      }
      printf("All good 1\n");
      for(unsigned long i = 0; i < find_fn->second.versions.size(); i++)
      {      
        unsigned long typeindex;
        printf("All good 2\n");
        if(!find_fn->second.versions[i].arguments.empty())
        {
          printf("All good 3\n");
          map<string,string> arguments = find_fn->second.versions[i].arguments;
          map<string,string>::iterator arg_it = arguments.begin();
          for(;arg_it != arguments.end(); arg_it++)
          {
            if(arg_it->second != types[typeindex])
            {
              printf("ERROR: Couldn't find the function.");
              Unwind(); 
            }
            typeindex++;
          }
          string out = get_unique_res(find_fn->second.versions[i].ret_type);
          if(find_fn->second.versions[i].fastcc) //If the function is fast or recursive
            out += " = tail call fastcc ";
          else
            out += " = call ccc ";
          out += func + " ";
          out += find_fn->second.versions[i].ret_type + " @";
          out += func + to_string(i) + "(";
          for(unsigned long argtypes = 0; argtypes < types.size(); argtypes++)
          {
            out += types[argtypes] + " " + get_res(argtypes) + ", ";
          }
          out = string(out,0,out.length()-1) + ")";
        }
        else if(cdr(form) == NULL)
        {
          string out = get_unique_res(find_fn->second.versions[i].ret_type);
          if(find_fn->second.versions[i].fastcc) //If the function is fast or recursive
            out += " = tail call fastcc ";
          else
            out += " = call ccc ";
          out += find_fn->second.versions[i].ret_type + " @";
          out += func + to_string(i) + "()";
          return out;
        }
        else
        {
          printf("ERORR: Couldn't find the function.");
          Unwind();
        }
      }
      printf("ERORR: Couldn't find the function.");
      Unwind();
    }
    printf("ERORR: Couldn't find the function.");
    Unwind();
  }*/