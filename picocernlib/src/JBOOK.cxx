#include "TFile.h"
#include "TH1F.h"
#include "TTree.h"
#include <iostream>
#include <map>
#include <string>
#include <sstream>
//http://stackoverflow.com/questions/3418231/c-replace-part-of-a-string-with-another-string
static void replace_all(std::string& str, const std::string& from, const std::string& to)
{
    if(from.empty())
        return;
    size_t start_pos = 0;
    while((start_pos = str.find(from, start_pos)) != std::string::npos)
        {
            str.replace(start_pos, from.length(), to);
            start_pos += to.length(); // In case 'to' contains 'from', like replacing 'x' with 'yx'
        }
}
//http://stackoverflow.com/questions/599989/is-there-a-built-in-way-to-split-strings-in-c
std::vector<std::string> tokenize(const std::string& str, const std::string& delimiters  )
{
    std::vector<std::string> tokens;
    // Skip delimiters at beginning.
    std::string::size_type lastPos = str.find_first_not_of(delimiters, 0);
    // Find first "non-delimiter".
    std::string::size_type pos     = str.find_first_of(delimiters, lastPos);

    while (std::string::npos != pos || std::string::npos != lastPos)
        {
            // Found a token, add it to the vector.
            tokens.push_back(str.substr(lastPos, pos - lastPos));
            // Skip delimiters.  Note the "not_of"
            lastPos = str.find_first_not_of(delimiters, pos);
            // Find next "non-delimiter"
            pos = str.find_first_of(delimiters, lastPos);
        }
    return tokens;
}
std::map<int, TH1F*> gmapTH1F;
std::map<int, TTree*> gmapTTree;
std::map<std::string, TFile*> gmapTFile;
extern "C" {


    void mzebra_(int *a) {}
    void mzpaw_(int *a) {}



    void hlimit_(int a) {}
    void hcdir_(char*,char*)
    {
//Do better
    }
    void hstaf_(char*) {}
    void hldir_(char*,char*) {gDirectory->ls();}
    void hbook1_(int* a, const char* titl, int* b, float* x1, float* x2) {gmapTH1F[*a]=new TH1F(Form("%i",a),titl,*b,*x1,*x2);}
    void hf1_(int* a,float* x, float* w) {if (gmapTH1F.find(*a)!=gmapTH1F.end())gmapTH1F[*a]->Fill(*x,*w);}
    int hexist_(int *a) {if (gmapTH1F.find(*a)!=gmapTH1F.end()) return 1; else return 0;}
    void hrput_(int a,char* n, char b)
    {
        std::string s(n);
        while (s.back()==' '||s.back()=='\n') s.pop_back();
        TFile* f= new TFile(Form("%s.root",s.c_str()),"recreate");
        f->cd();
        for (std::map<int, TH1F*>::iterator it=gmapTH1F.begin(); it!=gmapTH1F.end(); it++) it->second->Write();
        f->Close();
    }

    void hbarx_(int* a)
    {
        if (gmapTH1F.find(*a)!=gmapTH1F.end())
            gmapTH1F[*a]->Sumw2();
    }

    void  hbnt_(int* id,const char* t,const char* o)
    {
        gmapTTree[*id]= new TTree(Form("%i",*id),t);
    }

    void hpake_(int* a,float* xe)
    {
        if (gmapTH1F.find(*a)!=gmapTH1F.end())
            for (int i=0; i<gmapTH1F[*a]->GetNbinsX(); i++)
                gmapTH1F[*a]->SetBinError(i+1,xe[i]);

    }
    void hpak_(int* a,float* xe)
    {
        if (gmapTH1F.find(*a)!=gmapTH1F.end())
            for (int i=0; i<gmapTH1F[*a]->GetNbinsX(); i++)
                gmapTH1F[*a]->SetBinContent(i+1,xe[i]);
    }


    void hrout_(int* id,int*,char*)
    {
        if ((*id)==0)
            for (std::map<int, TH1F*>::iterator it=gmapTH1F.begin(); it!=gmapTH1F.end(); it++) it->second->Write();
        else if (gmapTH1F.find(*id)!=gmapTH1F.end())	gmapTH1F[*id]->Write();
    }

    void hrend_(char*dir)
    {
        std::string d(dir);
        while (d.back()==' '||d.back()=='\n') d.pop_back();
        if (gmapTFile.find(d)!=gmapTFile.end())
            {
                for (std::map<int, TTree*>::iterator it=gmapTTree.begin(); it!=gmapTTree.end(); it++) it->second->Write();
                gmapTFile[d]->Close();
            }
    }

    void hropen_(int* lun,const char* dir,const char*fname,const char* opt,
                 int* rec ,int* stat)
    {
        std::string d(dir);
        while (d.back()==' '||d.back()=='\n') d.pop_back();
        std::string f(fname);
        while (f.back()==' '||f.back()=='\n') f.pop_back();

        gmapTFile[d]=new TFile(Form("%s.root",f.c_str()),"recreate");

    }
    void  hfnt_(int* id)
    {
        if (gmapTTree.find(*id)!=gmapTTree.end())
            gmapTTree[*id]->Fill();
    }

    void hfill_(int*a ,float*x,float*y,float*w)
    {
        if (std::abs(*y)<0.000001) hf1_(a,x,w);
        else
            printf("Warning in hfill\n");
    }




    void hbname_(int*id ,const char* vname ,char*re ,const char* form,int,int)
    {
        std::string vn(vname);
        while (vn.back()==' '||vn.back()=='\n') vn.pop_back();
        std::string fo(form);
        while (fo.back()==' '||fo.back()=='\n') fo.pop_back();
//printf("in->|%s|<-\n",fo.c_str());

        std::map<std::string,size_t> sizes;
        sizes["b"]=sizeof(unsigned int);
        sizes["U"]=sizeof(unsigned int);
        sizes["F"]=sizeof(float);
        sizes["R"]=sizeof(float);
        sizes["I"]=sizeof(int);

        std::vector<std::vector<std::string> > vars;
        bool in_brackets_sq,in_brackets_ro;
        int n_vars=0;
        in_brackets_sq=false;
        in_brackets_ro=false;
        std::vector<std::string> def;
        def.push_back("");
        def.push_back("");
        def.push_back("");
        def.push_back("");
        def.push_back("");
        def.push_back("");
        vars.push_back(def);
        for (int i=0; i<fo.length(); i++)
            {
                if (fo.at(i)==','&&!in_brackets_ro&&!in_brackets_sq) { vars.push_back(def);  continue;}
                if (fo.at(i)=='(') in_brackets_ro=true;
                if (fo.at(i)==')') in_brackets_ro=false;
                if (fo.at(i)=='[') in_brackets_sq=true;
                if (fo.at(i)==']') in_brackets_sq=false;
                if (fo.at(i)==']'||fo.at(i)=='[') continue;
                if(in_brackets_sq) vars.back()[1]+=fo.at(i);
                else
                    vars.back()[0]+=fo.at(i);
            }

//for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin();it!=vars.end();it++)
//printf("->|%s|%s|%s|%s|%s|%s|\n",(*it)[0].c_str(),(*it)[1].c_str(),(*it)[2].c_str(),(*it)[3].c_str(),(*it)[4].c_str(),(*it)[5].c_str());

        for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin(); it!=vars.end(); it++)
            {
                (*it)[1].erase(0,(*it)[1].find_first_of(",")+1);

                (*it)[2]="I";
                if (  (*it)[0].find(":U")!=std::string::npos) (*it)[2]="b";
                if (  (*it)[0].find(":R")!=std::string::npos) (*it)[2]="F";
                if (  (*it)[0].find(":I")!=std::string::npos) (*it)[2]="I";
                replace_all((*it)[0],":U","");
                replace_all((*it)[0],":R","");
                replace_all((*it)[0],":I","");
                replace_all((*it)[0],":","");
            }
        for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin(); it!=vars.end(); it++)
            {
                replace_all((*it)[0],","," ");
                replace_all((*it)[0],")"," ");
                replace_all((*it)[0],"("," ");
                std::vector<std::string> x=tokenize((*it)[0]," ");
                (*it)[0]=x[0];
                for (int i=x.size()-1; i>0; i--) { (*it)[3]+= x[i];  (*it)[3]+=" ";}
            }
        for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin(); it!=vars.end(); it++)
            {
                int k=1;
                if ((*it)[3]!="")
                    {
                        std::vector<std::string> dims=tokenize((*it)[3]," ");
                        for (std::vector<std::string>:: iterator it2=dims.begin(); it2!=dims.end(); it2++)
                            {
                                std::string nn=*it2;
                                for (std::vector<std::vector<std::string> >  ::iterator it3=vars.begin(); it3!=vars.end(); it3++)
                                    if ((*it3)[0]==(*it2)) nn=(*it3)[1];
                                k*=(int)(std::stof(nn));
                            }
                    }
                char buf[20];
                sprintf(buf,"%i",k*sizes[(*it)[2]]);
                (*it)[4]=std::string(buf);
            }
        for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin(); it!=vars.end(); it++)
            {
                (*it)[5]=(*it)[0]+std::string("[")+(*it)[3];
                replace_all((*it)[5]," ","][");
                (*it)[5].pop_back();
                (*it)[5]+="/";
                (*it)[5]+=(*it)[2];
            }
//for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin();it!=vars.end();it++)
//printf("->|%s|%s|%s|%s|%s|%s|\n",(*it)[0].c_str(),(*it)[1].c_str(),(*it)[2].c_str(),(*it)[3].c_str(),(*it)[4].c_str(),(*it)[5].c_str());
        char* poi=re;
        for (std::vector<std::vector<std::string> >  ::iterator it=vars.begin(); it!=vars.end(); it++)
            {
                gmapTTree[*id]->Branch((*it)[0].c_str(),poi,(*it)[5].c_str());
                poi+=std::stoi((*it)[4]);
            }
    }
}
